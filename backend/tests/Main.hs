{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings, RankNTypes, TypeApplications, TupleSections #-}

import Control.Lens

import           Control.Monad.Except      (ExceptT (ExceptT), runExceptT)
import           Control.Monad.State       (evalStateT)
import           Data.Foldable             (foldrM, for_, or)
import           Data.Generics.Product     (field)
import           Data.Generics.Sum         (_As)
import           Data.List                 (inits)
import           Data.List.NonEmpty        (NonEmpty ((:|)))
import qualified Data.Map                  as Map
import           Data.Monoid               (Any (Any, getAny))
import           Data.Random.Source.StdGen (mkStdGen)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text, pack)
import           Numeric.Natural           (Natural)
import           Test.Tasty
import           Test.Tasty.HUnit

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
    let testProg = foldrM
          (\i (gs,_,_) -> inputEvent gs (ConfirmOk (PlayerId i)) Nothing)
          (Pregame roleConfirms, Nothing, Nothing)
          [1..5]
    in inputExpectation testProg (Right
      ( Rounds roundsState
      , Just $ ShufflePlayerOrder playerOrder
      , Just $ RoundsCommenced roundsState
      ))
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
  let decentRound1Proposal = Set.fromList [PlayerId 4, PlayerId 1]
      proposedState = Rounds (roundsState & field @"roundsCurrentProposal" .~ Proposed decentRound1Proposal Map.empty)
      approvedState = Rounds (roundsState & field @"roundsCurrentProposal" .~ Approved decentRound1Proposal Map.empty)
  in [ testGroup "Team Proposal" $
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
          ( proposedState & _As @"Rounds".field @"roundsCurrentProposal"._As @"Proposed"._2. at (PlayerId 1) ?~ False
          , Nothing
          ,Nothing
          ))
    , testCase "Voting Twice Doesn't work" $ error "todo"
    , testGroup "Last vote concludes team proposal" $
      [ testCase "Success moves to project vote" $ error "todo"
      , testCase "Fifth failure means Side-effects win" $ error "todo"
      ]
    , testCase "Voting doesn't work when team not-proposed or approved" $ error "todo"
    ]
  , testGroup "Project Voting"
    [ testCase "Voting works" $ error "todo"
    , testCase "Voting Twice Doesn't work" $ error "todo"
    , testCase "Only players in team can vote " $ error "todo"
    , testGroup "Last vote finalises round" $
      [ testCase "All success makes crusaders win" $ error "todo"
      , testCase "1 failure fails project on non 2 fail round" $ error "todo"
      , testCase "1 failure succeeds project on 2 fail round" $ error "todo"
      , testCase "2 failures fail project on a 2 fail round" $ error "todo"
      , testCase "3rd Crusader win moves to firing round" $ error "todo"
      , testCase "3rd Crusader win moves to side effects winning" $ error "todo"
      ]
    , testCase "Voting doesn't work when still in proposal" $ error "todo"
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
      inputTest (FiringRound roles) i (Left InvalidActionForGameState)
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

inputTest gs i = inputExpectation (inputEvent gs i Nothing)
inputExpectation prog e = do
  out <- runExceptT $ evalStateT prog (mkStdGen 1337)
  out @?= e

playersToMap = Map.fromList . fmap (\p -> (p^.field @"playerId", p))
player1 = Player (PlayerId 1) "Player1"
players = (\i -> Player (PlayerId i) ("Player" <> (pack . show $ i))) <$> [2..]
player2 = head players
player11 = players !! 11
roles = Map.fromList $ zipPlayersRoles (player1 : players) (playersToRoles Players5)
roleConfirms = (\(p,r) -> (p,r,False)) <$> roles
zipPlayersRoles = zipWith (\p@(Player pId _) r -> (pId,(p,r)))
playerOrder = PlayerId <$> (2 :| [4,5,1,3])
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
everyState =
  [ WaitingForPlayers player1 Map.empty
  , Pregame roleConfirms
  , Rounds roundsState
  , FiringRound roles
  , Complete CrusadersWin
  , Aborted (PlayerId 1)
  ]
