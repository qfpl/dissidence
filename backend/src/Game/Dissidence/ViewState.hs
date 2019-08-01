{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, TemplateHaskell, TypeApplications #-}
module Game.Dissidence.ViewState where

import Control.Lens

import           Control.Monad.Error.Lens  (throwing)
import           Control.Monad.Except      (MonadError, lift)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import           Data.Foldable             (fold)
import           Data.Generics.Product     (field)
import           Data.List                 (sortOn)
import qualified Data.Map                  as Map
import           Data.Maybe                (catMaybes)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Servant.Elm               (deriveBoth)

import Game.Dissidence.AesonOptions (ourAesonOptions)
import Game.Dissidence.Db
import Game.Dissidence.GameState    (EndCondition, GameStateInputEvent, GameStateOutputEvent,
                                     HistoricRoundState, LeadershipQueue, PlayerId, PlayersMap, Role (..),
                                     RoundShape, TeamVotingResult)
import Numeric.Natural              (Natural)
-- Eww. Prisms instead?
import qualified Game.Dissidence.GameState.Internal as GS

-- This dupe is very annoying. But servant-elm doesn't deal well with putting a type param in there


data CensoredRole = Censored | CensoredSideEffect deriving (Eq, Show, Generic)
deriveBoth ourAesonOptions ''CensoredRole

data ViewStateProposalState
  = NoProposal
  | Proposed (Set PlayerId) (Set PlayerId)
  | Approved (Set PlayerId) (Set PlayerId)
  deriving (Eq, Show, Generic)
deriveBoth ourAesonOptions ''ViewStateProposalState

data ViewStateCurrentRoundState = ViewStateCurrentRoundState
 { currentRoundShape    :: RoundShape
 , currentRoundProposal :: ViewStateProposalState
 , currentRoundVotes    :: [TeamVotingResult]
 } deriving (Eq,Show, Generic)
deriveBoth ourAesonOptions ''ViewStateCurrentRoundState


data ViewRoundsState = ViewRoundsState
  { roundsMyRole          :: Role
  , roundsRoles           :: PlayersMap CensoredRole
  , roundsCurrentLeader   :: PlayerId
  , roundsLeadershipQueue :: LeadershipQueue
  , roundsCurrent         :: ViewStateCurrentRoundState
  , roundsFuture          :: [RoundShape]
  , roundsHistoric        :: [HistoricRoundState]
  } deriving (Eq, Show, Generic)
deriveBoth ourAesonOptions ''ViewRoundsState

data ViewState
  = WaitingForPlayers PlayerId (Set PlayerId)
  | Pregame Role (PlayersMap CensoredRole) (PlayersMap Bool)
  | Rounds ViewRoundsState
  | FiringRound Role (PlayersMap CensoredRole) [HistoricRoundState]
  | Complete (PlayersMap Role) EndCondition [HistoricRoundState]
  | Aborted PlayerId
  deriving (Eq, Show, Generic)
deriveBoth ourAesonOptions ''ViewState

data DbViewState = DbViewState
  { dbViewStateGameId :: GameId
  , dbViewState       :: ViewState
  }
  deriving (Eq, Show, Generic)
deriveBoth ourAesonOptions ''DbViewState

data ViewStateOutputEvent
  = PlayerAdded PlayerId
  | PlayerRemoved PlayerId
  | PregameStarted (PlayersMap CensoredRole)
  | PlayerConfirmed PlayerId
  | RoundsCommenced ViewRoundsState
  | TeamProposed (Set PlayerId)
  | TeamApproved Natural
  | TeamRejected Natural PlayerId
  | NextRound Bool Natural
  | ThreeSuccessfulProjects
  | PlayerFired PlayerId
  | GameEnded (PlayersMap Role) EndCondition
  | GameAborted PlayerId
  | GameCrashed
  deriving (Generic, Show)
deriveBoth ourAesonOptions ''ViewStateOutputEvent

data NewViewStateEvent = NewViewStateEventChat Text | NewViewStateEventInput GameStateInputEvent deriving (Generic, Show)
deriveBoth ourAesonOptions ''NewViewStateEvent

data ViewStateEventData = ViewStateEventChat ChatLine | ViewStateEventOutput ViewStateOutputEvent deriving (Generic, Show)
deriveBoth ourAesonOptions ''ViewStateEventData

data ViewStateEvent = ViewStateEvent
  { viewStateEventTime   :: Posix
  , viewStateEventPlayer :: PlayerId
  , viewStateEventData   :: ViewStateEventData
  } deriving (Generic, Show)

deriveBoth ourAesonOptions ''ViewStateEvent

censorRole :: Role -> Role -> CensoredRole
censorRole playerRole otherRole = case (playerRole,otherRole) of
  (CompositionalCrusaders True, SneakySideEffects _) -> CensoredSideEffect
  (SneakySideEffects _, SneakySideEffects _)         -> CensoredSideEffect
  _                                                  -> Censored

censorRoles :: PlayerId -> PlayersMap Role -> PlayersMap CensoredRole
censorRoles pId roles = cr <$> (Map.delete pId roles)
  where
    cr = maybe (const Censored) censorRole  .  Map.lookup pId $ roles

myRole :: (MonadError e m, GS.AsGameStateInputError e) => PlayerId -> PlayersMap Role -> m Role
myRole pId roles =
  let myRoleMay = Map.lookup pId roles
  in case myRoleMay of
    Nothing -> throwing GS._PlayerNotInGame pId
    Just r  -> pure r

censorProposalState :: GS.ProposalState -> ViewStateProposalState
censorProposalState ps = case ps of
  GS.NoProposal          -> NoProposal
  GS.Proposed team votes -> Proposed team (Set.fromList $ Map.keys votes)
  GS.Approved team votes -> Approved team (Set.fromList $ Map.keys votes)

currentRoundStateToViewState :: GS.CurrentRoundState -> ViewStateCurrentRoundState
currentRoundStateToViewState rs = ViewStateCurrentRoundState
  (rs ^. field @"currentRoundShape")
  (rs ^. field @"currentRoundProposal".to censorProposalState)
  (rs ^. field @"currentRoundVotes")

roundsStateToViewState :: (MonadError e m, GS.AsGameStateInputError e) => PlayerId -> GS.RoundsState -> m ViewRoundsState
roundsStateToViewState pId rs = do
  myR <- myRole pId (rs ^. field @"roundsRoles")
  pure $ ViewRoundsState
    myR
    (censorRole myR <$> rs^.field @"roundsRoles")
    ( rs ^. field @"roundsCurrentLeader")
    ( rs ^. field @"roundsLeadershipQueue")
    ( rs ^. field @"roundsCurrent".to currentRoundStateToViewState)
    ( rs ^. field @"roundsFuture")
    ( rs ^. field @"roundsHistoric")

gameStateToViewState :: (MonadError e m, GS.AsGameStateInputError e) => PlayerId -> DbGameState -> m DbViewState
gameStateToViewState pId dgs = do
  vs <- case dgs ^. field @"dbGameState" of
    (GS.WaitingForPlayers owner ps)  -> pure $ WaitingForPlayers owner ps
    (GS.Pregame roles confirms)      -> do
      myR <- myRole pId roles
      pure $ Pregame myR (censorRoles pId roles) confirms
    (GS.Rounds rs)                   -> Rounds <$> roundsStateToViewState pId rs
    (GS.FiringRound roles history)   -> do
      myR <- myRole pId roles
      pure $ FiringRound myR (censorRoles pId roles) history
    (GS.Complete roles endC history) -> pure $ Complete roles endC history
    (GS.Aborted aborter)             -> pure $ Aborted aborter
  pure $ DbViewState (dgs^.field @"dbGameStateId") vs

gameEventToViewEvent
  :: (MonadError e m, GS.AsGameStateInputError e)
  => PlayerId
  -> GameStateOutputEvent
  -> m ViewStateOutputEvent
gameEventToViewEvent pId gsoe = case gsoe of
  GS.PlayerAdded added         -> pure $ PlayerAdded added
  GS.PlayerRemoved removed     -> pure $ PlayerAdded removed
  GS.PregameStarted roles      -> pure $ PregameStarted (censorRoles pId roles)
  GS.PlayerConfirmed confirmed -> pure $ PlayerConfirmed confirmed
  GS.RoundsCommenced rs        -> RoundsCommenced <$> roundsStateToViewState pId rs
  GS.TeamProposed a            -> pure $ TeamProposed a
  GS.TeamApproved a            -> pure $ TeamApproved a
  GS.TeamRejected a b          -> pure $ TeamRejected a b
  GS.NextRound a b             -> pure $ NextRound a b
  GS.ThreeSuccessfulProjects   -> pure $ ThreeSuccessfulProjects
  GS.PlayerFired a             -> pure $ PlayerFired a
  GS.GameEnded a b             -> pure $ GameEnded a b
  GS.GameAborted a             -> pure $ GameAborted a
  GS.GameCrashed               -> pure GameCrashed


gameEventsToViewEvents
  :: (MonadError e m, GS.AsGameStateInputError e)
  => PlayerId
  -> [ChatLine]
  -> [DbGameStateEvent]
  -> m [ViewStateEvent]
gameEventsToViewEvents pId cls gses = do
  gsvses <- traverse sub gses
  pure $ sortOn (^.field @"viewStateEventTime") . fold $
    [ (\cl -> ViewStateEvent (cl ^. field @"chatLineTime") (cl ^. field @"chatLinePlayerId") (ViewStateEventChat cl)) <$> cls
    , catMaybes gsvses
    ]
  where
    sub gse = runMaybeT $ do
      out <- MaybeT . pure $ gse ^? field @"dbGameStateEventOutput" ._Just
      ve <- lift $ gameEventToViewEvent pId out
      pure $  ViewStateEvent (gse^.field @"dbGameStateEventTime") (gse^.field @"dbGameStatePlayerId") (ViewStateEventOutput ve)

