{-# LANGUAGE OverloadedStrings, TypeApplications, DataKinds, RankNTypes, FlexibleContexts #-}

import Control.Lens

import qualified Data.Map as Map
import Data.Foldable (for_, or)
import Data.Monoid (Any(Any,getAny))
import Control.Monad.State (evalStateT)
import Data.List (inits)
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
                let expectedRoles = Map.fromList . zipPlayersRoles (player1 : players) $ 
                        [ CompositionalCrusaders Nothing
                        , SneakySideEffects Nothing 
                        , SneakySideEffects (Just MiddleManager)
                        , CompositionalCrusaders Nothing
                        , CompositionalCrusaders (Just FPExpert)
                        ]
                in inputTest
                    (WaitingForPlayers player1 (playersToMap (take 4 players)))
                    (StartGame (PlayerId 1))
                    (Right 
                        (Pregame expectedRoles
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
        []
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
    inputTest gs i e = do
        out <- evalStateT (inputEvent gs i) (mkStdGen 1337)
        out @?= e

    playersToMap = Map.fromList . fmap (\p -> (p^.field @"playerId", p))
    player1 = Player (PlayerId 1) "Player1"
    players = (\i -> Player (PlayerId i) ("Player" <> (pack . show $ i))) <$> [2..]
    player2 = head players
    player11 = players !! 11
    roles = Map.fromList $ zipPlayersRoles players (playersToRoles Players5)
    zipPlayersRoles = zipWith (\p@(Player pId _) r -> (pId,(p,r)))
    roundsState = initialRoundsState roles Players5
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
        , Pregame roles
        , Rounds roundsState
        , FiringRound roles
        , Complete CrusadersWin
        , Aborted (PlayerId 1)
        ]