{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as Map
import Data.Foldable (for_)
import Data.List (inits)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Numeric.Natural (Natural)
import Test.Tasty
import Test.Tasty.HUnit

import Game.Dissidence.GameState 

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "inputEvent :: Waiting For Players"
  [ testGroup "AddPlayer"
    [ testCase "Add one to a game of 1" $ 
        inputEvent 
          (WaitingForPlayers player1 Map.empty) 
          (AddPlayer player2) 
          @?= 
            Right (WaitingForPlayers player1 (playersToMap (take 1 players)),Nothing, Just $ PlayerAdded player2)
    , testCase "Adds all the way up to 10 players" $
        for_ (take 8 . drop 2 . inits $ players) $ \(p:ps) -> 
          inputEvent 
            (WaitingForPlayers player1 (playersToMap ps)) 
            (AddPlayer p) 
            @?= 
              Right (WaitingForPlayers player1 (playersToMap (p:ps)),Nothing, Just $ PlayerAdded p)
    , testCase "11th player errors" $
        inputEvent 
          (WaitingForPlayers player1 (playersToMap . take 9 $ players))
          (AddPlayer player11)
          @?= 
            Left GameIsFull
    ]
  , testGroup "RemovePlayer" $
    [ testCase "Removing owner should error" $
        inputEvent (WaitingForPlayers player1 Map.empty) (RemovePlayer (PlayerId 1))
        @?=
          Left GameOwnerCannotLeave
    , testCase "Removing non owner should work" $
        inputEvent (WaitingForPlayers player1 (playersToMap [player2])) (RemovePlayer (PlayerId 2))
        @?=
          Right (WaitingForPlayers player1 Map.empty, Nothing, Just $ PlayerRemoved (PlayerId 2))
    ]
  ]
  where 
    playersToMap = Map.fromList . fmap (\(Player pId pName) -> (pId,pName))
    player1 = Player (PlayerId 1) "Player1"
    players = (\i -> Player (PlayerId i) ("Player" <> (pack . show $ i))) <$> [2..]
    player2 = head players
    player11 = players !! 11