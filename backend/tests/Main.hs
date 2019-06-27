{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Numeric.Natural (Natural)
import Test.Tasty
import Test.Tasty.HUnit

import Game.Dissidence.GameState 

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "inputEvent"
  [ testCase "No more than 10 players added" $
    inputEvent 
      (WaitingForPlayers player1 Map.empty) 
      (AddPlayer player2) 
      @?= 
        Right (WaitingForPlayers player1 (playersToMap [player2]),Nothing, Just $ PlayerAdded player2)
  ]
  where 
    playersToMap = Map.fromList . fmap (\(Player pId pName) -> (pId,pName))
    player1 = Player (PlayerId 1) "Player1"
    player2 = Player (PlayerId 2) "Player2"