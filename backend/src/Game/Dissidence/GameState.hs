{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeApplications, DataKinds #-}
module Game.Dissidence.GameState where

import Control.Lens

import GHC.Generics (Generic)
import Data.Generics.Product (field)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import Data.Text (Text)

newtype PlayerId = PlayerId { unPlayerId :: Natural } deriving (Eq, Ord, Show, Generic)

data Player = Player 
  { playerId :: PlayerId
  , playerName :: Text 
  } deriving (Eq, Ord, Show, Generic)

data CrusaderRole = FPExpert deriving (Eq, Ord, Show, Generic)
data SideEffectRole = MiddleManager deriving (Eq, Ord, Show, Generic)

data Role 
   = CompositionalCrusaders (Maybe CrusaderRole) 
   | SneakySideEffects (Maybe SideEffectRole) 
   deriving (Eq, Ord, Show, Generic)

data Round = Round1 | Round2 | Round3 | Round4 | Round5 deriving (Eq, Show, Generic)

type PlayersMap = Map PlayerId Player

data SideEffectWinCondition = FPExpertFired | ThreeFailedProjects | FiveTeamsVetoed
  deriving (Eq, Show, Generic)

data EndCondition = CrusadersWin | SideEffectsWin SideEffectWinCondition | GameCancelled
  deriving (Eq, Show, Generic)

data GameState 
    = WaitingForPlayers Player (Map PlayerId Text)
    | Pregame (Map PlayerId Role)
    | Rounds RoundState 
    | FiringRound 
    | Complete EndCondition
    deriving (Eq, Show, Generic)

data VotingResult = VotingResult
  { votingResultTeam :: Set Player
  , votingResult     :: Map PlayerId Bool
  } deriving (Eq, Show, Generic)

data RoundState = RoundState
  { roundTeamSize :: Natural
  , roundTeam     :: Maybe (Set PlayerId)
  , roundVotes    :: [VotingResult]
  } deriving (Eq, Show, Generic)

-- These are the events given to us by the UI and the ones stored in the database. 
-- We can't just ship these to the UI
data GameStateInputEvent 
    = AddPlayer Player 
    | RemovePlayer Player
    | GameStart PlayerId
    | ConfirmOk PlayerId
    | ProposeTeam (Set PlayerId)
    | VoteOnTeam PlayerId Bool
    | VoteOnProject PlayerId Bool
    | FirePlayer PlayerId
    | GameQuit PlayerId
    deriving (Eq, Show, Generic)

-- These are the bits where the game decides some kind of random event 
data GameStateInternalEvent 
    = AssignRoles (Map PlayerId Role) 
    | AssignLeader Player
    deriving (Eq, Show, Generic)

-- These are the events given back to the UI communicating the change that it needs to update.
-- Some of these events contain more information than a single player should get and need to be filtered 
-- based on the player before sending them out. 
data GameStateOutputEvent 
    = PlayerAdded Player
    | PlayerRemoved Player
    | PregameStart (Map PlayerId Role)
    | InitialLeader Player
    | ProposedTeam (Set Player)
    | TeamApproved Natural
    | TeamRejected Natural Player
    | NextRound Round Bool 
    | ThreeSuccessfulProjects
    | PlayerFired Player
    | GameEnded EndCondition
    deriving (Eq, Show, Generic)

inputEvent 
  :: GameState 
  -> GameStateInputEvent 
  -> Either Text (GameState, Maybe GameStateInternalEvent, Maybe GameStateOutputEvent)
inputEvent gs ev = case gs of 
  WaitingForPlayers o ps -> case ev of 
    AddPlayer p@(Player pId pName) -> 
      if Map.member pId ps || (o ^. field @"playerId") == pId
      then Right (gs, Nothing, Nothing) 
      else Right (WaitingForPlayers o (Map.insert pId pName ps), Nothing, Just $ PlayerAdded p)
    _ -> undefined
  _ -> undefined