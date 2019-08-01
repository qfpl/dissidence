{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings, RankNTypes, TupleSections, TypeApplications #-}

import Control.Lens
import Test.Tasty
import Test.Tasty.HUnit

import           Control.Monad.Except       (Except, runExcept)
import           Control.Monad.State        (StateT, evalStateT)
import           Data.Algorithm.DiffContext (getContextDiff, prettyContextDiff)
import           Data.Foldable              (foldrM, for_)
import           Data.Generics.Product      (field)
import           Data.Generics.Sum          (_As)
import           Data.List                  (inits)
import           Data.List.NonEmpty         (NonEmpty ((:|)))
import qualified Data.List.NonEmpty         as NEL
import qualified Data.Map                   as Map
import           Data.Random.Source.StdGen  (StdGen, mkStdGen)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (pack)
import           Numeric.Natural            (Natural)
import qualified Text.PrettyPrint           as Doc
import           Text.Show.Pretty           (ppShow)

import Game.Dissidence.GameState
import Game.Dissidence.GameState.Internal

main :: IO ()
main = defaultMain inputEventTests

waitingForPlayersTests :: TestTree
waitingForPlayersTests = testGroup "Waiting For Players" $
  [ testGroup "AddPlayer" $
    [ testCase "Add one to a game of 1" $
      inputTest
        (WaitingForPlayers player1 Set.empty)
        player2
        AddPlayer
        (Right (WaitingForPlayers player1 (Set.fromList (take 1 players)),Nothing, Just $ PlayerAdded player2))
    , testCase "Adds all the way up to 10 players" $
     for_ (take 8 . drop 2 . inits $ players) $ \(p:ps) ->
       inputTest
         (WaitingForPlayers player1 (Set.fromList ps))
         p
         AddPlayer
         (Right (WaitingForPlayers player1 (Set.fromList (p:ps)),Nothing, Just $ PlayerAdded p))
    , testCase "11th player errors" $
     inputTest
       (WaitingForPlayers player1 (Set.fromList . take 9 $ players))
       player11
       AddPlayer
       (Left GameIsFull)
    ]
  , testGroup "RemovePlayer" $
    [ testCase "Removing owner should error" $
      inputTest (WaitingForPlayers player1 Set.empty) (natToPId 1) RemovePlayer
        (Left GameOwnerCannotLeave)
        , testCase "Removing non owner should work" $
          inputTest (WaitingForPlayers player1 (Set.fromList [player2])) (natToPId 2) RemovePlayer
            (Right (WaitingForPlayers player1 Set.empty, Nothing, Just $ PlayerRemoved (natToPId 2)))
    ]
  , testGroup "StartGame" $
    [ testCase "Cannot start game with less than 5 players" $ do
      inputTest
        (WaitingForPlayers player1 Set.empty)
        (natToPId 1)
        StartGame
        (Left NotEnoughPlayers)
      for_ (take 3 . drop 1 . inits $ players) $ \ps ->
        inputTest
          (WaitingForPlayers player1 (Set.fromList ps))
          (natToPId 1)
          StartGame
          (Left NotEnoughPlayers)
    , testCase "!Owner cannot start" $
      for_ [2..10] $ \i ->
        inputTest
          (WaitingForPlayers player1 (Set.fromList (take 9 players)))
          (natToPId i)
          StartGame
          (Left OwnerMustStartGame)
    , testCase "Owner starts game" $
      -- This is set to our StdGen seed of 1337
      let expectedRoles = Map.fromList . zip (player1 : players) $
           [ SneakySideEffects True
           , CompositionalCrusaders False
           , SneakySideEffects False
           , CompositionalCrusaders True
           , CompositionalCrusaders False
           ]
      in inputTest
        (WaitingForPlayers player1 (Set.fromList (take 4 players)))
        (natToPId 1)
        StartGame
        (Right
          (Pregame expectedRoles (Map.fromList $ zip (player1 : (take 4 players)) (repeat False))
          , Just $ AssignRoles expectedRoles
          , Just $ PregameStarted expectedRoles
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
      inputTest (WaitingForPlayers player1 Set.empty) (natToPId 2) i (Left InvalidActionForGameState)
  ]

pregameTests :: TestTree
pregameTests = testGroup "Pregame" $
  [ testCase "Confirming for player not in game errors" $
    inputTest (Pregame roles Map.empty) (natToPId 10) ConfirmOk
      (Left $ PlayerNotInGame (natToPId 10))
  , testCase "Confirming works" $
    inputTest
      (Pregame roles Map.empty)
      (natToPId 1)
      ConfirmOk
      (Right
        ( Pregame roles (Map.singleton (natToPId 1) True)
        , Nothing
        , Just $ PlayerConfirmed (natToPId 1)))
  , testCase "Confirming all players starts game" $
    inputsTest (Pregame roles Map.empty) ((,ConfirmOk) . natToPId <$> 1 :| [2..5]) $ Right
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
      inputTest (Pregame roles Map.empty) (natToPId 2) i (Left InvalidActionForGameState)
  ]

roundsTests :: TestTree
roundsTests = testGroup "Round" $
  [ testGroup "Team Proposal" $
    [ testCase "Proposing Team works" $
      inputTest (Rounds roundsState) (natToPId 2) (ProposeTeam decentRound1Proposal)
        (Right (proposedState, Nothing, Just $ TeamProposed decentRound1Proposal))
    , testCase "Proposing Team doesn't work for not leader" $
      inputTest (Rounds roundsState) (natToPId 1) (ProposeTeam (pIdSet [1,2]))
        (Left PlayerIsNotLeader)
    , testCase "Proposing Team that is not the correct size errors" $
      let playerIds = natToPId <$> [1,2,3,4,5]
      in for_ [0,1,3,4,5] $ \n ->
        inputTest (Rounds roundsState) (natToPId 2) (ProposeTeam (Set.fromList $ take n playerIds))
          (Left IncorrectTeamSize)
    , testCase "Proposing Team doesn't work when team proposed" $
      inputTest
        proposedState
        (natToPId 1)
        (ProposeTeam decentRound1Proposal)
        (Left $ InvalidActionForGameState)
    , testCase "Proposing Team doesn't work when team approved" $
      inputTest
        approvedState
        (natToPId 1)
        (ProposeTeam decentRound1Proposal)
        (Left $ InvalidActionForGameState)
    ]
  , testGroup "Team Voting"
    [ testCase "Voting works" $
      inputTest proposedState (natToPId 1) (VoteOnTeam False)
        (Right
          ( proposedState & _As @"Rounds".roundsCurrentProposal._As @"Proposed"._2. at (natToPId 1) ?~ False
          , Nothing
          , Nothing
          ))
    , testCase "Leader doesn't vote on their own team" $
      inputTest proposedState (natToPId 2) (VoteOnTeam False)
        (Left LeaderCannotVoteOnTeam)
    , testCase "Voting Twice Doesn't work" $
       inputsTest proposedState ((natToPId 1, VoteOnTeam False) :| [(natToPId 1, VoteOnTeam True)] )
        (Left DuplicateVote)
    , testGroup "Last vote concludes team proposal" $
      let voteInputs = fmap (\(pId,b) -> (natToPId pId,VoteOnTeam b))
      in
        [ testCase "Success moves to project vote" $
          let votes = (1,True) :| [(3,True),(4,True),(5,True)]
              votesMap = Map.fromList . NEL.toList . fmap (_1 %~ natToPId) $ votes
          in inputsTest proposedState (voteInputs votes) $ Right
            ( approvedState & _As @"Rounds".roundsCurrentVotes <>~
              [TeamVotingResult (natToPId 2) decentRound1Proposal votesMap]
            , Nothing
            , Just $ TeamApproved 4
            )
        , testCase "Team Vote Failure cycles leader" $
          let votes = (1,False) :| [(3,False),(4,True),(5,True)]
              votesMap = Map.fromList . NEL.toList . fmap (_1 %~ natToPId) $ votes
          in inputsTest proposedState (voteInputs votes) $ Right
            ( proposedState
              & _As @"Rounds".roundsCurrentProposal .~ NoProposal
              & _As @"Rounds".field @"roundsCurrentLeader" .~ (natToPId 4)
              & _As @"Rounds".field @"roundsLeadershipQueue"._Wrapped .~ (natToPId <$> 5 :| [1,3,2,4])
              & _As @"Rounds".roundsCurrentVotes <>~
                [TeamVotingResult (natToPId 2) decentRound1Proposal votesMap]
            , Nothing
            , Just $ TeamRejected 2 (natToPId 4)
            )
        , testCase "Fifth failure means Side-effects win" $
          let votes = (1,False) :| [(3,False),(4,True),(5,True)]
              votesMap = Map.fromList . NEL.toList . fmap (_1 %~ natToPId) $ votes
              votesFailMap = Map.fromList . fmap (\pId -> (natToPId pId, False))
          in inputsTest
            (proposedState & _As @"Rounds" . roundsCurrentVotes .~
              [ TeamVotingResult (natToPId 4) decentRound1Proposal (votesFailMap [1,2,3,5])
              , TeamVotingResult (natToPId 5) decentRound1Proposal (votesFailMap [1,2,3,4])
              , TeamVotingResult (natToPId 1) decentRound1Proposal (votesFailMap [2,3,4,5])
              , TeamVotingResult (natToPId 3) decentRound1Proposal (votesFailMap [1,2,4,5])
              ]
            )
            (voteInputs votes)
            $ Right
            ( Complete roles (SideEffectsWin FiveTeamsVetoed) [HistoricRoundState (RoundShape 2 False) Nothing
              [ TeamVotingResult (natToPId 4) decentRound1Proposal (votesFailMap [1,2,3,5])
              , TeamVotingResult (natToPId 5) decentRound1Proposal (votesFailMap [1,2,3,4])
              , TeamVotingResult (natToPId 1) decentRound1Proposal (votesFailMap [2,3,4,5])
              , TeamVotingResult (natToPId 3) decentRound1Proposal (votesFailMap [1,2,4,5])
              , TeamVotingResult (natToPId 2) decentRound1Proposal votesMap
              ] RoundNoConsensus]
            , Nothing
            , Just $ GameEnded roles (SideEffectsWin FiveTeamsVetoed)
            )
        ]
    , testCase "Voting doesn't work when team not-proposed or approved" $
      for_ [Rounds roundsState, approvedState] $ \s ->
        inputTest s (natToPId 1) (VoteOnTeam True) (Left InvalidActionForGameState)
    ]
  , testGroup "Project Voting"
    [ testCase "Voting works" $
      inputTest approvedState (natToPId 4) (VoteOnProject True) (Right
        ( approvedState &_As @"Rounds".roundsCurrentProposal._As @"Approved"._2.at (natToPId 4) ?~ True
        , Nothing
        , Nothing
        ))
    , testCase "Voting Twice Doesn't work" $
       inputsTest approvedState ((natToPId 1, VoteOnProject False) :| [(natToPId 1, VoteOnProject True)] )
        (Left DuplicateVote)
    , testCase "Only players in team can vote " $
      inputTest approvedState (natToPId 2) (VoteOnProject True) (Left PlayerNotInTeam)
    , testGroup "Last vote finalises round" $
      [ testCase "All success makes crusaders win" $
        inputsTest approvedState ((natToPId 1, VoteOnProject True) :| [(natToPId 4, VoteOnProject True)])
          (Right
            (Rounds (RoundsState roles (natToPId 4) (lqueue $ 5 :| [1,3,2,4])
              (CurrentRoundState (RoundShape 3 False) NoProposal [])
              [RoundShape 3 False, RoundShape 2 False, RoundShape 3 False, RoundShape 3 False]
              [HistoricRoundState (RoundShape 2 False) (Just decentRound1Proposal) [] (RoundSuccess 0)]
            )
            , Nothing
            , Just $ NextRound True 0
            ))
      , testCase "1 failure fails project on normal round" $
        inputsTest approvedState ((natToPId 1, VoteOnProject False) :| [(natToPId 4, VoteOnProject True)]) $
          (Right
            (Rounds (RoundsState roles (natToPId 4) (lqueue $ 5 :| [1,3,2,4])
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
          ((natToPId 1, VoteOnProject False) :| [(natToPId 4, VoteOnProject True)])
          (Right
            (Rounds (RoundsState roles (natToPId 4) (lqueue $ 5 :| [1,3,2,4])
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
          ((natToPId 1, VoteOnProject False) :| [(natToPId 4, VoteOnProject False)])
          (Right
            (Rounds (RoundsState roles (natToPId 4) (lqueue $ 5 :| [1,3,2,4])
              (CurrentRoundState (RoundShape 3 False) NoProposal [])
              [RoundShape 3 False, RoundShape 2 False, RoundShape 3 False, RoundShape 3 False]
              [HistoricRoundState (RoundShape 2 True) (Just decentRound1Proposal) [] (RoundFailure 2)]
            )
            , Nothing
            , Just $ NextRound False 2
            ))
      , testCase "3rd Win moves to firing round" $
        inputsTest finalRound
          ((natToPId 1, VoteOnProject True) :| [(natToPId 2,VoteOnProject True), (natToPId 3, VoteOnProject True)])
          ( Right
            (FiringRound roles
                [ HistoricRoundState (RoundShape 2 False) Nothing [] (RoundSuccess 0)
                , HistoricRoundState (RoundShape 3 False) Nothing [] (RoundFailure 0)
                , HistoricRoundState (RoundShape 2 False) Nothing [] (RoundSuccess 0)
                , HistoricRoundState (RoundShape 3 False) Nothing [] (RoundFailure 0)
                , HistoricRoundState (RoundShape 3 False) (Just $ pIdSet [1,2,3]) [] (RoundSuccess 0)
                ]
            , Nothing
            , Just ThreeSuccessfulProjects))
      , testCase "3rd Fail moves to side effects winning" $
        inputsTest finalRound
          ((natToPId 1, VoteOnProject True) :| [(natToPId 2, VoteOnProject True), (natToPId 3, VoteOnProject False)])
          ( Right
            (Complete roles (SideEffectsWin ThreeFailedProjects)
                [ HistoricRoundState (RoundShape 2 False) Nothing [] (RoundSuccess 0)
                , HistoricRoundState (RoundShape 3 False) Nothing [] (RoundFailure 0)
                , HistoricRoundState (RoundShape 2 False) Nothing [] (RoundSuccess 0)
                , HistoricRoundState (RoundShape 3 False) Nothing [] (RoundFailure 0)
                , HistoricRoundState (RoundShape 3 False) (Just $ pIdSet [1,2,3]) [] (RoundFailure 1)
                ]
            , Nothing
            , Just $ GameEnded roles (SideEffectsWin ThreeFailedProjects)))
      ]
    , testCase "Voting doesn't work when still in proposal" $
      for_ [Rounds roundsState, proposedState] $ \s ->
        inputTest s (natToPId 1) (VoteOnProject True) (Left InvalidActionForGameState)
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
          inputTest s (natToPId 2) i (Left InvalidActionForGameState)
  ]

firingRoundTests :: TestTree
firingRoundTests = testGroup "FiringRound" $
  [ testCase "Players other than the middle manager can't vote" $
    inputTest firingRound (natToPId 1) (FirePlayer (natToPId 2)) (Left PlayerNotManager)
  , testCase "Middle manager fires FP manager. Side Effects Win" $
    inputTest firingRound (natToPId 2) (FirePlayer (natToPId 1)) $ Right
      ( Complete roles (SideEffectsWin FPExpertFired) firingRoundH
      , Nothing
      , Just $ GameEnded roles (SideEffectsWin FPExpertFired)
      )
  , testCase "Middle manager fires Anyone Else. Crusaders Win" $
    inputTest firingRound (natToPId 2) (FirePlayer (natToPId 4)) $ Right
      ( Complete roles CrusadersWin firingRoundH , Nothing , Just $ GameEnded roles CrusadersWin)
  , testCase "Invalid Inputs are Rejected" $
    let validInput i = all ($ i)
         [ isn't (_As @"FirePlayer")
         , isn't (_As @"AbortGame")
         ]
    in for_ (filter validInput everyInput) $ \i ->
      inputTest (FiringRound roles []) (natToPId 2) i (Left InvalidActionForGameState)
  ]
  where
    firingRound = FiringRound roles firingRoundH
    firingRoundH =
      [ HistoricRoundState (RoundShape 2 False) Nothing [] (RoundSuccess 0)
      , HistoricRoundState (RoundShape 3 False) Nothing [] (RoundFailure 0)
      , HistoricRoundState (RoundShape 2 False) Nothing [] (RoundSuccess 0)
      , HistoricRoundState (RoundShape 3 False) Nothing [] (RoundSuccess 0)
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
      inputTest s (natToPId 1) AbortGame (Right (Aborted (natToPId 1), Nothing, Just $ GameAborted (natToPId 1)))
  , testGroup "Complete" $
    [ testCase "All inputs rejected" $
      for_ everyInput $ \i -> inputTest (Aborted (natToPId 1)) (natToPId 2) i (Left InvalidActionForGameState)
    ]
  , testGroup "Aborted" $
    [ testCase "All inputs rejected" $
      for_ everyInput $ \i -> inputTest (Aborted (natToPId 1)) (natToPId 2) i (Left InvalidActionForGameState)
    ]
  ]

inputsTest
  :: GameState
  -> NonEmpty (PlayerId, GameStateInputEvent)
  -> Either GameStateInputError (GameState, Maybe GameStateInternalEvent, Maybe GameStateOutputEvent)
  -> IO ()
inputsTest gs is = inputExpectation (foldrM (\(pId, i) (gs',_,_) -> inputEvent gs' pId i Nothing) (gs, Nothing,Nothing) is)

inputTest
  :: GameState
  -> PlayerId
  -> GameStateInputEvent
  -> Either GameStateInputError (GameState, Maybe GameStateInternalEvent, Maybe GameStateOutputEvent)
  -> IO ()
inputTest gs pId i = inputExpectation (inputEvent gs pId i Nothing)

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

natToPId :: Natural -> PlayerId
natToPId = PlayerId . ("Player" <>) . pack . show

players :: [PlayerId]
players = pids [2..]

player1, player2, player11 :: PlayerId
player1 = PlayerId "Player1"
player2 = head players
player11 = players !! 11

roles :: Map.Map PlayerId Role
roles = Map.fromList $ zip (player1 : players) (playersToRoles Players5)

pids :: Functor f => f Natural -> f PlayerId
pids = fmap natToPId

lqueue :: NonEmpty Natural -> LeadershipQueue
lqueue = LeadershipQueue . pids

pIdSet :: [Natural] -> Set PlayerId
pIdSet = Set.fromList . pids

playerOrder :: LeadershipQueue
playerOrder = lqueue (2 :| [4,5,1,3])
roundsState :: RoundsState
roundsState = initialRoundsState roles playerOrder Players5

everyInput :: [GameStateInputEvent]
everyInput =
  [ AddPlayer
  , RemovePlayer
  , StartGame
  , ConfirmOk
  , ProposeTeam (pIdSet [1])
  , VoteOnTeam True
  , VoteOnProject True
  , FirePlayer (natToPId 1)
  , AbortGame
  ]

decentRound1Proposal :: Set (PlayerId)
decentRound1Proposal = pIdSet [4, 1]
proposedState :: GameState
proposedState = Rounds (roundsState & roundsCurrentProposal .~ Proposed decentRound1Proposal Map.empty)
approvedState :: GameState
approvedState = Rounds (roundsState & roundsCurrentProposal .~ Approved decentRound1Proposal Map.empty)

finalRound :: GameState
finalRound = Rounds $ RoundsState
  roles
  (natToPId 2)
  (lqueue (4 :| [5,1,3,2]))
  (CurrentRoundState (RoundShape 3 False) (Approved (pIdSet [1..3]) Map.empty) [])
  []
  [ HistoricRoundState (RoundShape 2 False) Nothing [] (RoundSuccess 0)
  , HistoricRoundState (RoundShape 3 False) Nothing [] (RoundFailure 0)
  , HistoricRoundState (RoundShape 2 False) Nothing [] (RoundSuccess 0)
  , HistoricRoundState (RoundShape 3 False) Nothing [] (RoundFailure 0)
  ]

everyState :: [GameState]
everyState =
  [ WaitingForPlayers player1 Set.empty
  , Pregame roles Map.empty
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
  , Aborted (natToPId 1)
  ]
