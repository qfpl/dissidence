{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings, RankNTypes, TupleSections, TypeApplications #-}

import Control.Lens

import           Control.Error              (flipEither, hush)
import           Control.Monad.Except       (Except, ExceptT (ExceptT), MonadError, runExcept, runExceptT)
import           Control.Monad.State        (MonadState, State, StateT, evalStateT, get, put, runStateT)
import           Control.Monad.Writer       (MonadWriter, Writer, runWriter, tell)
import           Data.Algorithm.DiffContext (getContextDiff, prettyContextDiff)
import           Data.Foldable              (foldrM, for_, or)
import           Data.Generics.Product      (field)
import           Data.Generics.Sum          (_As)
import           Data.List                  (inits)
import           Data.List.NonEmpty         (NonEmpty ((:|)))
import qualified Data.List.NonEmpty         as NEL
import qualified Data.Map                   as Map
import           Data.Monoid                (Any (Any, getAny))
import           Data.Random.Source.StdGen  (StdGen, mkStdGen)
import           Data.RVar                  (MonadRandom)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text, pack)
import           Numeric.Natural            (Natural)
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Text.PrettyPrint           as Doc
import           Text.Show.Pretty           (ppShow)

import Game.Dissidence.GameState

main :: IO ()
main = defaultMain inputEventTests

waitingForPlayersTests :: TestTree
waitingForPlayersTests = testGroup "Waiting For Players" $
  [ testGroup "AddPlayer" $
    [ testCase "Add one to a game of 1" $
      inputTest
        (WaitingForPlayers player1 Map.empty)
        (AddPlayer player2)
        (Right (WaitingForPlayers player1 (playersToMap (take 1 players)),Nothing, Just $ PlayerAdded player2))
    , testCase "Adds all the way up to 10 players" $
     for_ (take 8 . drop 2 . inits $ players) $ \(p:ps) ->
       inputTest
         (WaitingForPlayers player1 (playersToMap ps))
         (AddPlayer p)
         (Right (WaitingForPlayers player1 (playersToMap (p:ps)),Nothing, Just $ PlayerAdded p))
    , testCase "11th player errors" $
     inputTest
       (WaitingForPlayers player1 (playersToMap . take 9 $ players))
       (AddPlayer player11)
       (Left GameIsFull)
    ]
  , testGroup "RemovePlayer" $
    [ testCase "Removing owner should error" $
      inputTest (WaitingForPlayers player1 Map.empty) (RemovePlayer (PlayerId 1))
        (Left GameOwnerCannotLeave)
        , testCase "Removing non owner should work" $
          inputTest (WaitingForPlayers player1 (playersToMap [player2])) (RemovePlayer (PlayerId 2))
            (Right (WaitingForPlayers player1 Map.empty, Nothing, Just $ PlayerRemoved (PlayerId 2)))
    ]
  , testGroup "StartGame" $
    [ testCase "Cannot start game with less than 5 players" $ do
      inputTest
        (WaitingForPlayers player1 Map.empty)
        (StartGame (PlayerId 1))
        (Left NotEnoughPlayers)
      for_ (take 3 . drop 1 . inits $ players) $ \ps ->
        inputTest
          (WaitingForPlayers player1 (playersToMap ps))
          (StartGame (PlayerId 1))
          (Left NotEnoughPlayers)
    , testCase "!Owner cannot start" $
      for_ [2..10] $ \i ->
        inputTest
          (WaitingForPlayers player1 (playersToMap (take 9 players)))
          (StartGame (PlayerId i))
          (Left OwnerMustStartGame)
    , testCase "Owner starts game" $
      -- This is set to our StdGen seed of 1337
      let expectedRoles = Map.fromList . zipPlayersRoles (player1 : players) $
           [ SneakySideEffects (Just MiddleManager)
           , CompositionalCrusaders Nothing
           , SneakySideEffects Nothing
           , CompositionalCrusaders (Just FPExpert)
           , CompositionalCrusaders Nothing
           ]
      in inputTest
        (WaitingForPlayers player1 (playersToMap (take 4 players)))
        (StartGame (PlayerId 1))
        (Right
          (Pregame ((\(p,r) -> (p,r,False)) <$> expectedRoles)
          , Just $ AssignRoles ((^._2) <$> expectedRoles)
          , Just $ PregameStarted ((^._2) <$> expectedRoles)
        ))
    ]

  , testCase "Invalid Inputs are Rejected" $
    let validInput i = all ($ i)
         [ isn't (_As @"AddPlayer")
         , isn't (_As @"RemovePlayer")
         , isn't (_As @"StartGame")
         , isn't (_As @"AbortGame")
         ]
    in for_ (filter validInput everyInput) $ \i ->
      inputTest (WaitingForPlayers player1 Map.empty) i (Left InvalidActionForGameState)
  ]

pregameTests :: TestTree
pregameTests = testGroup "Pregame" $
  [ testCase "Confirming for player not in game errors" $
    inputTest (Pregame roleConfirms) (ConfirmOk $ PlayerId 10)
      (Left $ PlayerNotInGame (PlayerId 10))
  , testCase "Confirming works" $
    inputTest
      (Pregame roleConfirms)
      (ConfirmOk $ PlayerId 1)
      (Right
        ( Pregame (Map.adjust (_3 .~ True) (PlayerId 1) roleConfirms)
        , Nothing
        , Just $ PlayerConfirmed ))
  , testCase "Confirming all players starts game" $
    inputsTest (Pregame roleConfirms) (ConfirmOk . PlayerId <$> 1 :| [2..5]) $ Right
      ( Rounds roundsState
      , Just $ ShufflePlayerOrder playerOrder
      , Just $ RoundsCommenced roundsState
      )
  , testCase "Invalid Inputs are Rejected" $
    let validInput i = all ($ i)
         [ isn't (_As @"ConfirmOk")
         , isn't (_As @"AbortGame")
         ]
    in for_ (filter validInput everyInput) $ \i ->
      inputTest (Pregame roleConfirms) i (Left InvalidActionForGameState)
  ]

roundsTests :: TestTree
roundsTests = testGroup "Round" $
  [ testGroup "Team Proposal" $
    [ testCase "Proposing Team works" $
      inputTest (Rounds roundsState) (ProposeTeam (PlayerId 2) decentRound1Proposal)
        (Right (proposedState, Nothing, Just $ TeamProposed decentRound1Proposal))
    , testCase "Proposing Team doesn't work for not leader" $
      inputTest (Rounds roundsState) (ProposeTeam (PlayerId 1) (Set.fromList [PlayerId 1, PlayerId 2]))
        (Left PlayerIsNotLeader)
    , testCase "Proposing Team that is not the correct size errors" $
      let playerIds = PlayerId <$> [1,2,3,4,5]
      in for_ [0,1,3,4,5] $ \n ->
        inputTest (Rounds roundsState) (ProposeTeam (PlayerId 2) (Set.fromList $ take n playerIds))
          (Left IncorrectTeamSize)
    , testCase "Proposing Team doesn't work when team proposed" $
      inputTest
        proposedState
        (ProposeTeam (PlayerId 1) decentRound1Proposal)
        (Left $ InvalidActionForGameState)
    , testCase "Proposing Team doesn't work when team approved" $
      inputTest
        approvedState
        (ProposeTeam (PlayerId 1) decentRound1Proposal)
        (Left $ InvalidActionForGameState)
    ]
  , testGroup "Team Voting"
    [ testCase "Voting works" $
      inputTest proposedState (VoteOnTeam (PlayerId 1) False)
        (Right
          ( proposedState & _As @"Rounds".roundsCurrentProposal._As @"Proposed"._2. at (PlayerId 1) ?~ False
          , Nothing
          , Nothing
          ))
    , testCase "Leader doesn't vote on their own team" $
      inputTest proposedState (VoteOnTeam (PlayerId 2) False)
        (Left LeaderCannotVoteOnTeam)
    , testCase "Voting Twice Doesn't work" $
       inputsTest proposedState ((VoteOnTeam (PlayerId 1) False) :| [VoteOnTeam (PlayerId 1) True] )
        (Left DuplicateVote)
    , testGroup "Last vote concludes team proposal" $
      let voteInputs = fmap (\(pId,b) -> VoteOnTeam (PlayerId pId) b)
      in
        [ testCase "Success moves to project vote" $
          let votes = (1,True) :| [(3,True),(4,True),(5,True)]
              votesMap = Map.fromList . fmap (_1 %~ PlayerId) . NEL.toList $ votes
          in inputsTest proposedState (voteInputs votes) $ Right
            ( approvedState & _As @"Rounds".roundsCurrentVotes <>~
              [TeamVotingResult (PlayerId 2) decentRound1Proposal votesMap]
            , Nothing
            , Just $ TeamApproved 4
            )
        , testCase "Team Vote Failure cycles leader" $
          let votes = (1,False) :| [(3,False),(4,True),(5,True)]
              votesMap = Map.fromList . fmap (_1 %~ PlayerId) . NEL.toList $ votes
          in inputsTest proposedState (voteInputs votes) $ Right
            ( proposedState
              & _As @"Rounds".roundsCurrentProposal .~ NoProposal
              & _As @"Rounds".field @"roundsLeadershipQueue" .~ (PlayerId <$> 4 :| [5,1,3,2])
              & _As @"Rounds".roundsCurrentVotes <>~
                [TeamVotingResult (PlayerId 2) decentRound1Proposal votesMap]
            , Nothing
            , Just $ TeamRejected 2 (PlayerId 4)
            )
        , testCase "Fifth failure means Side-effects win" $
          let votes = (1,False) :| [(3,False),(4,True),(5,True)]
              votesMap = Map.fromList . fmap (_1 %~ PlayerId) . NEL.toList $ votes
              votesFailMap = Map.fromList . fmap (\i -> (PlayerId i, False))
          in inputsTest
            (proposedState & _As @"Rounds" . roundsCurrentVotes .~
              [ TeamVotingResult (PlayerId 4) decentRound1Proposal (votesFailMap [1,2,3,5])
              , TeamVotingResult (PlayerId 5) decentRound1Proposal (votesFailMap [1,2,3,4])
              , TeamVotingResult (PlayerId 1) decentRound1Proposal (votesFailMap [2,3,4,5])
              , TeamVotingResult (PlayerId 3) decentRound1Proposal (votesFailMap [1,2,4,5])
              ]
            )
            (voteInputs votes)
            $ Right
            ( Complete roles (SideEffectsWin FiveTeamsVetoed) [HistoricRoundState (RoundShape 2 False) Nothing
              [ TeamVotingResult (PlayerId 4) decentRound1Proposal (votesFailMap [1,2,3,5])
              , TeamVotingResult (PlayerId 5) decentRound1Proposal (votesFailMap [1,2,3,4])
              , TeamVotingResult (PlayerId 1) decentRound1Proposal (votesFailMap [2,3,4,5])
              , TeamVotingResult (PlayerId 3) decentRound1Proposal (votesFailMap [1,2,4,5])
              , TeamVotingResult (PlayerId 2) decentRound1Proposal votesMap
              ] RoundNoConsensus]
            , Nothing
            , Just $ GameEnded roles (SideEffectsWin FiveTeamsVetoed)
            )
        ]
    , testCase "Voting doesn't work when team not-proposed or approved" $
      for_ [Rounds roundsState, approvedState] $ \s ->
        inputTest s (VoteOnTeam (PlayerId 1) True) (Left InvalidActionForGameState)
    ]
  , testGroup "Project Voting"
    [ testCase "Voting works" $
      inputTest approvedState (VoteOnProject (PlayerId 4) True) (Right
        ( approvedState &_As @"Rounds".roundsCurrentProposal._As @"Approved"._2.at (PlayerId 4) ?~ True
        , Nothing
        , Nothing
        ))
    , testCase "Voting Twice Doesn't work" $
       inputsTest approvedState ((VoteOnProject (PlayerId 1) False) :| [VoteOnProject (PlayerId 1) True] )
        (Left DuplicateVote)
    , testCase "Only players in team can vote " $
      inputTest approvedState (VoteOnProject (PlayerId 2) True) (Left PlayerNotInTeam)
    , testGroup "Last vote finalises round" $
      [ testCase "All success makes crusaders win" $
        inputsTest approvedState ((VoteOnProject (PlayerId 1) True) :| [VoteOnProject (PlayerId 4) True])
          (Right
            (Rounds (RoundsState roles (pids $ 4 :| [5,1,3,2])
              (CurrentRoundState (RoundShape 3 False) NoProposal [])
              [RoundShape 3 False, RoundShape 2 False, RoundShape 3 False, RoundShape 3 False]
              [HistoricRoundState (RoundShape 2 False) (Just decentRound1Proposal) [] (RoundSuccess 0)]
            )
            , Nothing
            , Just $ NextRound True 0
            ))
      , testCase "1 failure fails project on normal round" $
        inputsTest approvedState ((VoteOnProject (PlayerId 1) False) :| [VoteOnProject (PlayerId 4) True]) $
          (Right
            (Rounds (RoundsState roles (pids $ 4 :| [5,1,3,2])
              (CurrentRoundState (RoundShape 3 False) NoProposal [])
              [RoundShape 3 False, RoundShape 2 False, RoundShape 3 False, RoundShape 3 False]
              [HistoricRoundState (RoundShape 2 False) (Just decentRound1Proposal) [] (RoundFailure 1)]
            )
            , Nothing
            , Just $ NextRound False 1
            ))
      , testCase "1 failure succeeds project on 2 fail round" $
        inputsTest
          (approvedState & _As @"Rounds" . roundsCurrentShape . field @"roundShapeTwoFails" .~ True)
          ((VoteOnProject (PlayerId 1) False) :| [VoteOnProject (PlayerId 4) True])
          (Right
            (Rounds (RoundsState roles (pids $ 4 :| [5,1,3,2])
              (CurrentRoundState (RoundShape 3 False) NoProposal [])
              [RoundShape 3 False, RoundShape 2 False, RoundShape 3 False, RoundShape 3 False]
              [HistoricRoundState (RoundShape 2 True) (Just decentRound1Proposal) [] (RoundSuccess 1)]
            )
            , Nothing
            , Just $ NextRound True 1
            ))
      , testCase "2 failures fail project on a 2 fail round" $
        inputsTest
          (approvedState & _As @"Rounds" . roundsCurrentShape . field @"roundShapeTwoFails" .~ True)
          ((VoteOnProject (PlayerId 1) False) :| [VoteOnProject (PlayerId 4) False])
          (Right
            (Rounds (RoundsState roles (pids $ 4 :| [5,1,3,2])
              (CurrentRoundState (RoundShape 3 False) NoProposal [])
              [RoundShape 3 False, RoundShape 2 False, RoundShape 3 False, RoundShape 3 False]
              [HistoricRoundState (RoundShape 2 True) (Just decentRound1Proposal) [] (RoundFailure 2)]
            )
            , Nothing
            , Just $ NextRound False 2
            ))
      , testCase "3rd Win moves to firing round" $
        inputsTest finalRound
          ((VoteOnProject (PlayerId 1) True) :| [VoteOnProject (PlayerId 2) True, VoteOnProject (PlayerId 3) True])
          ( Right
            (FiringRound roles
                [ HistoricRoundState (RoundShape 2 False) Nothing [] (RoundSuccess 0)
                , HistoricRoundState (RoundShape 3 False) Nothing [] (RoundFailure 0)
                , HistoricRoundState (RoundShape 2 False) Nothing [] (RoundSuccess 0)
                , HistoricRoundState (RoundShape 3 False) Nothing [] (RoundFailure 0)
                , HistoricRoundState (RoundShape 3 False) (Just $ Set.fromList (pids [1,2,3])) [] (RoundSuccess 0)
                ]
            , Nothing
            , Just ThreeSuccessfulProjects))
      , testCase "3rd Fail moves to side effects winning" $
        inputsTest finalRound
          ((VoteOnProject (PlayerId 1) True) :| [VoteOnProject (PlayerId 2) True, VoteOnProject (PlayerId 3) False])
          ( Right
            (Complete roles (SideEffectsWin ThreeFailedProjects)
                [ HistoricRoundState (RoundShape 2 False) Nothing [] (RoundSuccess 0)
                , HistoricRoundState (RoundShape 3 False) Nothing [] (RoundFailure 0)
                , HistoricRoundState (RoundShape 2 False) Nothing [] (RoundSuccess 0)
                , HistoricRoundState (RoundShape 3 False) Nothing [] (RoundFailure 0)
                , HistoricRoundState (RoundShape 3 False) (Just $ Set.fromList (pids [1,2,3])) [] (RoundFailure 1)
                ]
            , Nothing
            , Just $ GameEnded roles (SideEffectsWin ThreeFailedProjects)))
      ]
    , testCase "Voting doesn't work when still in proposal" $
      for_ [Rounds roundsState, proposedState] $ \s ->
        inputTest s (VoteOnProject (PlayerId 1) True) (Left InvalidActionForGameState)
    ]
  , testCase "Invalid Inputs are Rejected" $
    let validInput i = all ($ i)
         [ isn't (_As @"ProposeTeam")
         , isn't (_As @"VoteOnTeam")
         , isn't (_As @"VoteOnProject")
         , isn't (_As @"AbortGame")
         ]
    in for_ (filter validInput everyInput) $ \i ->
        for_ [Rounds roundsState, proposedState, approvedState] $ \s ->
          inputTest s i (Left InvalidActionForGameState)
  ]

firingRoundTests :: TestTree
firingRoundTests = testGroup "FiringRound" $
  [ testCase "Players other than the middle manager can't vote" $ error "todo"
  , testCase "Middle manager fires FP manager. Side Effects Win" $ error "todo"
  , testCase "Middle manager fires Anyone Else. Crusaders Win" $ error "TODO"
  , testCase "Invalid Inputs are Rejected" $
    let validInput i = all ($ i)
         [ isn't (_As @"FirePlayer")
         , isn't (_As @"AbortGame")
         ]
    in for_ (filter validInput everyInput) $ \i ->
      inputTest (FiringRound roles []) i (Left InvalidActionForGameState)
  ]

inputEventTests :: TestTree
inputEventTests = testGroup "inputEvent" $
  [ waitingForPlayersTests
  , pregameTests
  , roundsTests
  , firingRoundTests
  , testCase "State that are not {Complete, Aborted} can be aborted" $
    let validState i = all ($ i)
         [ isn't (_As @"Complete")
         , isn't (_As @"Aborted")
         ]
    in for_ (filter validState everyState) $ \s ->
      inputTest s (AbortGame (PlayerId 1)) (Right (Aborted (PlayerId 1), Nothing, Just $ GameAborted (PlayerId 1)))
  , testGroup "Complete" $
    [ testCase "All inputs rejected" $
      for_ everyInput $ \i -> inputTest (Aborted (PlayerId 1)) i (Left InvalidActionForGameState)
    ]
  , testGroup "Aborted" $
    [ testCase "All inputs rejected" $
      for_ everyInput $ \i -> inputTest (Aborted (PlayerId 1)) i (Left InvalidActionForGameState)
    ]
  ]

inputsTest
  :: GameState
  -> NonEmpty GameStateInputEvent
  -> Either GameStateInputError (GameState, Maybe GameStateInternalEvent, Maybe GameStateOutputEvent)
  -> IO ()
inputsTest gs is = inputExpectation (foldrM (\i (gs,_,_) -> inputEvent gs i Nothing) (gs, Nothing,Nothing) is)

inputTest
  :: GameState
  -> GameStateInputEvent
  -> Either GameStateInputError (GameState, Maybe GameStateInternalEvent, Maybe GameStateOutputEvent)
  -> IO ()
inputTest gs i = inputExpectation (inputEvent gs i Nothing)

inputExpectation
  :: (Eq a, Show a)
  => StateT StdGen (Except GameStateInputError) a
  -> Either GameStateInputError a
  -> IO ()
inputExpectation prog e = runExcept (notRandom prog) @?~ e

(@?~)
  :: (Eq a, Show a)
  => a
  -> a
  -> IO ()
got @?~ expected = assertBool (show diff) (got == expected)
  where
    gotPurdy = lines $ ppShow got
    expectedPurdy = lines $ ppShow expected
    diff = prettyContextDiff "Got" "Expected" Doc.text (getContextDiff 3 gotPurdy expectedPurdy)

notRandom :: Monad m => StateT StdGen m a -> m a
notRandom = flip evalStateT (mkStdGen 1337)

playersToMap = Map.fromList . fmap (\p -> (p^.field @"playerId", p))
player1 = Player (PlayerId 1) "Player1"
players = (\i -> Player (PlayerId i) ("Player" <> (pack . show $ i))) <$> [2..]
player2 = head players
player11 = players !! 11
roles = Map.fromList $ zipPlayersRoles (player1 : players) (playersToRoles Players5)
roleConfirms = (\(p,r) -> (p,r,False)) <$> roles
zipPlayersRoles = zipWith (\p@(Player pId _) r -> (pId,(p,r)))

pids :: Functor f => f Natural -> f PlayerId
pids = fmap PlayerId

playerOrder = pids (2 :| [4,5,1,3])
roundsState = initialRoundsState roles playerOrder Players5
everyInput =
  [ AddPlayer player2
  , RemovePlayer (PlayerId 2)
  , StartGame (PlayerId 2)
  , ConfirmOk (PlayerId 2)
  , ProposeTeam (PlayerId 1) (Set.fromList [PlayerId 1])
  , VoteOnTeam (PlayerId 2) True
  , VoteOnProject (PlayerId 2) True
  , FirePlayer (PlayerId 2)
  , AbortGame (PlayerId 2)
  ]

decentRound1Proposal = Set.fromList [PlayerId 4, PlayerId 1]
proposedState = Rounds (roundsState & roundsCurrentProposal .~ Proposed decentRound1Proposal Map.empty)
approvedState = Rounds (roundsState & roundsCurrentProposal .~ Approved decentRound1Proposal Map.empty)

finalRound = Rounds $ RoundsState
  roles
  playerOrder
  (CurrentRoundState (RoundShape 3 False) (Approved (Set.fromList $ PlayerId <$> [1..3]) Map.empty) [])
  []
  [ HistoricRoundState (RoundShape 2 False) Nothing [] (RoundSuccess 0)
  , HistoricRoundState (RoundShape 3 False) Nothing [] (RoundFailure 0)
  , HistoricRoundState (RoundShape 2 False) Nothing [] (RoundSuccess 0)
  , HistoricRoundState (RoundShape 3 False) Nothing [] (RoundFailure 0)
  ]

everyState =
  [ WaitingForPlayers player1 Map.empty
  , Pregame roleConfirms
  , Rounds roundsState
  , proposedState
  , approvedState
  , FiringRound roles [ HistoricRoundState (RoundShape 2 False) Nothing [] (RoundSuccess 0)
    , HistoricRoundState (RoundShape 3 False) Nothing [] (RoundFailure 0)
    , HistoricRoundState (RoundShape 2 False) Nothing [] (RoundSuccess 0)
    , HistoricRoundState (RoundShape 3 False) Nothing [] (RoundSuccess 0)
    ]
  , Complete roles CrusadersWin [ HistoricRoundState (RoundShape 2 False) Nothing [] (RoundSuccess 0)
    , HistoricRoundState (RoundShape 3 False) Nothing [] (RoundFailure 0)
    , HistoricRoundState (RoundShape 2 False) Nothing [] (RoundSuccess 0)
    , HistoricRoundState (RoundShape 3 False) Nothing [] (RoundSuccess 0)
    ]
  , Aborted (PlayerId 1)
  ]
