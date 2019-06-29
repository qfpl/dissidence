{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeApplications, DataKinds, LambdaCase #-}
module Game.Dissidence.GameState where

-- See https://www.ultraboardgames.com/avalon/game-rules.php for deets

import Control.Lens

import GHC.Generics (Generic)
import Data.Generics.Product (field)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.RVar (MonadRandom, sampleRVar)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Random.Extras (shuffle)
import Numeric.Natural (Natural)
import Data.Text (Text)

newtype PlayerId = PlayerId { unPlayerId :: Natural } deriving (Eq, Ord, Show, Generic)

type RolesMap = Map PlayerId (Player, Role)

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

data PlayersCount = Players5 | Players6 | Players7 | Players8 | Players9 | Players10 
  deriving (Eq, Show, Generic)

data Round = Round1 | Round2 | Round3 | Round4 | Round5 deriving (Eq, Show, Generic)

type PlayersMap = Map PlayerId Player

data SideEffectWinCondition = FPExpertFired | ThreeFailedProjects | FiveTeamsVetoed
  deriving (Eq, Show, Generic)

data EndCondition = CrusadersWin | SideEffectsWin SideEffectWinCondition | GameCancelled
  deriving (Eq, Show, Generic)

data GameState 
    = WaitingForPlayers Player (Map PlayerId Player)
    | Pregame RolesMap
    | Rounds RoundsState
    | FiringRound RolesMap
    | Complete EndCondition
    | Aborted PlayerId
    deriving (Eq, Show, Generic)

data RoundsState = RoundsState
  { currentRound :: Round
  , roundsRoles  :: RolesMap
  , round1       :: RoundState
  , round2       :: RoundState
  , round3       :: RoundState
  , round4       :: RoundState
  , round5       :: RoundState
  } deriving (Eq, Show, Generic)

data VotingResult = VotingResult
  { votingResultTeam :: Set Player
  , votingResult     :: Map PlayerId Bool
  } deriving (Eq, Show, Generic)

data RoundState = RoundState
  { roundTeamSize    :: Natural
  , requiresTwoFails :: Bool
  , roundTeam        :: Maybe (Set PlayerId)
  , roundVotes       :: [VotingResult]
  } deriving (Eq, Show, Generic)

-- These are the events given to us by the UI and the ones stored in the database. 
-- We can't just ship these to the UI
data GameStateInputEvent 
    = AddPlayer Player 
    | RemovePlayer PlayerId
    | StartGame PlayerId
    | ConfirmOk PlayerId
    | ProposeTeam (Set PlayerId)
    | VoteOnTeam PlayerId Bool
    | VoteOnProject PlayerId Bool
    | FirePlayer PlayerId
    | AbortGame PlayerId
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
    | PlayerRemoved PlayerId
    | PregameStarted (Map PlayerId Role)
    | InitialLeader Player
    | ProposedTeam (Set Player)
    | TeamApproved Natural
    | TeamRejected Natural Player
    | NextRound Round Bool 
    | ThreeSuccessfulProjects
    | PlayerFired Player
    | GameEnded EndCondition
    | GameAborted PlayerId
    deriving (Eq, Show, Generic)

data GameStateInputError
  = GameIsFull
  | GameOwnerCannotLeave
  | InvalidActionForGameState
  | OwnerMustStartGame
  | NotEnoughPlayers
  deriving (Eq, Show, Generic)

inputEvent 
  :: MonadRandom m 
  => GameState 
  -> GameStateInputEvent 
  -> m (Either GameStateInputError (GameState, Maybe GameStateInternalEvent, Maybe GameStateOutputEvent))
inputEvent gs ev = case gs of 
  WaitingForPlayers o ps -> case ev of 
      AddPlayer p@(Player pId pName) -> pure $
          if Map.member pId ps || (o ^. field @"playerId") == pId then Right (gs, Nothing, Nothing) 
          else if Map.size ps >= 9 then Left GameIsFull
          else Right (WaitingForPlayers o (Map.insert pId p ps), Nothing, Just $ PlayerAdded p)

      RemovePlayer  pId -> pure $
        if (o ^. field @"playerId") == pId then Left GameOwnerCannotLeave
        else Right (WaitingForPlayers o (Map.delete pId ps), Nothing, Just $ PlayerRemoved pId)

      StartGame pId -> 
          if (o ^. field @"playerId") /= pId then pure $ Left OwnerMustStartGame
          else case (playersCount . (+1) . Map.size $ ps) of
              Nothing -> pure $ Left NotEnoughPlayers
              Just pc -> do
                roles <- sampleRVar . shuffle $ playersToRoles pc
                let rolesPlayerMap = Map.fromList $ 
                      zipWith (\p@(Player pId _) r -> (pId,(p,r))) (o:Map.elems ps) roles
                let rolesMap = fmap (^._2) rolesPlayerMap
                pure . Right $ 
                  ( Pregame rolesPlayerMap
                  , Just $ AssignRoles rolesMap
                  , Just $ PregameStarted rolesMap
                  )

      AbortGame pId -> abortGame pId

      _ -> invalidAction

  Aborted _ -> invalidAction
  where 
    abortGame pId = pure $ Right (Aborted pId, Nothing, Just $ GameAborted pId)
    invalidAction = pure . Left $ InvalidActionForGameState

-- Don't forget the owner! :)
playersCount :: Int -> Maybe PlayersCount
playersCount = \case 
    5  -> Just Players5
    6  -> Just Players6
    7  -> Just Players7
    8  -> Just Players8
    9  -> Just Players9
    10 -> Just Players10
    _ -> Nothing

playersToRoles :: PlayersCount -> [Role]
playersToRoles Players5 =
  [ CompositionalCrusaders (Just FPExpert)
  , SneakySideEffects (Just MiddleManager)
  , CompositionalCrusaders Nothing
  , CompositionalCrusaders Nothing
  , SneakySideEffects Nothing
  ]
playersToRoles Players6 = CompositionalCrusaders Nothing : playersToRoles Players5
playersToRoles Players7 = SneakySideEffects Nothing : playersToRoles Players6
playersToRoles Players8 = CompositionalCrusaders Nothing : playersToRoles Players7
playersToRoles Players9 = CompositionalCrusaders Nothing : playersToRoles Players8
playersToRoles Players10 = SneakySideEffects Nothing : playersToRoles Players9

initialRoundsState :: RolesMap -> PlayersCount -> RoundsState
initialRoundsState roles = \case 
  Players5  -> rss 2 3 2 3 3 False
  Players6  -> rss 2 3 4 3 4 False
  Players7  -> rss 2 3 3 4 4 True
  Players8  -> rss 3 4 4 5 5 True
  Players9  -> rss 3 4 4 5 5 True
  Players10 -> rss 3 4 4 5 5 True
  where 
    rss r1 r2 r3 r4 r5 b = 
      RoundsState Round1 roles (rs r1 False) (rs r2 False) (rs r3 False) (rs r4 b) (rs r5 False)
    rs n b = RoundState n b Nothing []