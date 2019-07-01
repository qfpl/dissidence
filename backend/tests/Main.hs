{-# LANGUAGE OverloadedStrings, TypeApplications, DataKinds, RankNTypes, FlexibleContexts #-}

import Control.Lens

import Control.Monad.Except (ExceptT(ExceptT),runExceptT)
import qualified Data.Map as Map
import Data.Foldable (for_, or, foldrM)
import Data.Monoid (Any(Any,getAny))
import Control.Monad.State (evalStateT)
import Data.List (inits)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Numeric.Natural (Natural)
import Data.Generics.Product (field)
import Data.Generics.Sum (_As)
import Data.Random.Source.StdGen (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit

import Game.Dissidence.GameState 

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "inputEvent" $ 
    [ testGroup "Waiting For Players" $
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
        , testCase "AbortGame aborts game" $
            inputTest (WaitingForPlayers player1 Map.empty) (AbortGame (PlayerId 1))
                (Right (Aborted (PlayerId 1), Nothing, Just $ GameAborted (PlayerId 1)))

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
    , testGroup "Pregame" $ 
        [ testCase "Confirming for player not in game errors" $
            inputTest (Pregame roleConfirms) (ConfirmOk $ PlayerId 10)
                (Left $ PlayerNotInGame)
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
                    (\i (gs,_,_) -> ExceptT $ inputEvent gs (ConfirmOk (PlayerId i)) Nothing) 
                    (Pregame roleConfirms, Nothing, Nothing) 
                    [1..5]
            in inputExpectation (runExceptT testProg) (Right 
                ( Rounds roundsState
                , Just $ ShufflePlayerOrder playerOrder
                , Just $ RoundsCommenced roundsState
                ))
        ]
    , testGroup "Round" $ 
        []
    , testGroup "FiringRound" $ 
        []
    , testGroup "Complete" $ 
        []
    , testGroup "Aborted" $ 
        [ testCase "All inputs rejected" $ 
            for_ everyInput $ \i -> inputTest (Aborted (PlayerId 1)) i (Left InvalidActionForGameState)
        ]
    ]
  where 
    inputTest gs i = inputExpectation (inputEvent gs i Nothing)
    inputExpectation prog e = do
        out <- evalStateT prog (mkStdGen 1337)
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
        , ProposeTeam (Set.fromList [PlayerId 1])
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