{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
{-# LANGUAGE TypeApplications, TypeFamilies                                                             #-}
module Game.Dissidence.GameState.Internal where

import Control.Lens

import           Control.Arrow         ((***))
import           Data.Aeson            (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import           Data.Aeson.TH         (deriveJSON)
import           Data.Generics.Product (field)
import           Data.Generics.Sum     (_As)
import           Data.List.NonEmpty    (NonEmpty)
import qualified Data.List.NonEmpty    as NEL
import           Data.Map              (Map)
import           Data.Monoid           (Sum (Sum), getSum)
import           Data.Proxy            (Proxy (Proxy))
import           Data.Set              (Set)
import           Data.Text             (Text)
import           Elm.Derive            (deriveBoth)
import           Elm.TyRep             (EPrimAlias (EPrimAlias), ETCon (ETCon), EType (ETyApp, ETyCon),
                                        ETypeDef (ETypePrimAlias), ETypeName (ETypeName),
                                        IsElmDefinition (..), toElmType)
import           GHC.Generics          (Generic)
import           Numeric.Natural       (Natural)

import Game.Dissidence.AesonOptions (ourAesonOptions)

newtype PlayerId = PlayerId { unPlayerId :: Text } deriving (Eq, Ord, Show, Generic, ToJSONKey, FromJSONKey)
deriveBoth ourAesonOptions ''PlayerId

data CrusaderRole = FPExpert deriving (Eq, Ord, Show, Generic)
deriveBoth ourAesonOptions ''CrusaderRole
data SideEffectRole = MiddleManager deriving (Eq, Ord, Show, Generic)
deriveBoth ourAesonOptions ''SideEffectRole

type PlayersMap a = Map PlayerId a

data Role
   = CompositionalCrusaders (Maybe CrusaderRole)
   | SneakySideEffects (Maybe SideEffectRole)
   deriving (Eq, Ord, Show, Generic)
deriveBoth ourAesonOptions ''Role

data PlayersCount = Players5 | Players6 | Players7 | Players8 | Players9 | Players10
  deriving (Eq, Show, Generic)

data SideEffectWinCondition = FPExpertFired | ThreeFailedProjects | FiveTeamsVetoed
  deriving (Eq, Show, Generic)
deriveBoth ourAesonOptions ''SideEffectWinCondition

data EndCondition = CrusadersWin | SideEffectsWin SideEffectWinCondition | GameCancelled
  deriving (Eq, Show, Generic)
deriveBoth ourAesonOptions ''EndCondition

data RoundShape = RoundShape
  { roundShapeTeamSize :: Natural
  , roundShapeTwoFails :: Bool
  } deriving (Eq, Show, Generic)
deriveBoth ourAesonOptions ''RoundShape

data ProposalState
  = NoProposal
  | Proposed (Set PlayerId) (PlayersMap Bool)
  | Approved (Set PlayerId) (PlayersMap Bool)
  deriving (Eq, Show, Generic)
deriveBoth ourAesonOptions ''ProposalState

data TeamVotingResult = TeamVotingResult
  { votingTeamLeader :: PlayerId
  , votingResultTeam :: Set PlayerId
  , votingResult     :: PlayersMap Bool
  } deriving (Eq, Show, Generic)
deriveBoth ourAesonOptions ''TeamVotingResult

data RoundResult = RoundSuccess Natural | RoundFailure Natural | RoundNoConsensus deriving (Eq, Show, Generic)
deriveBoth ourAesonOptions ''RoundResult

data CurrentRoundState = CurrentRoundState
 { currentRoundShape    :: RoundShape
 , currentRoundProposal :: ProposalState
 , currentRoundVotes    :: [TeamVotingResult]
 } deriving (Eq,Show, Generic)
deriveBoth ourAesonOptions ''CurrentRoundState

data HistoricRoundState = HistoricRoundState
  { historicRoundShape  :: RoundShape
  , historicRoundTeam   :: Maybe (Set PlayerId)
  , historicRoundVotes  :: [TeamVotingResult]
  , historicRoundResult :: RoundResult
  } deriving (Eq, Show, Generic)
deriveBoth ourAesonOptions ''HistoricRoundState

newtype LeadershipQueue = LeadershipQueue { unLeadershipQueue :: NonEmpty PlayerId } deriving (Eq,Show, Generic)
makeWrapped ''LeadershipQueue
instance FromJSON LeadershipQueue where
  parseJSON = (maybe (fail "NonEmpty list given empty list") (pure . LeadershipQueue) . NEL.nonEmpty =<<). parseJSON
instance ToJSON LeadershipQueue where
  toJSON = toJSON . NEL.toList . unLeadershipQueue

instance IsElmDefinition LeadershipQueue where
  compileElmDef _ = ETypePrimAlias
    (EPrimAlias
      (ETypeName "LeadershipQueue" [])
      (ETyApp (ETyCon (ETCon "List")) (toElmType (Proxy :: Proxy String))))

data RoundsState = RoundsState
  { roundsRoles           :: PlayersMap Role
  , roundsLeadershipQueue :: LeadershipQueue
  , roundsCurrent         :: CurrentRoundState
  , roundsFuture          :: [RoundShape]
  , roundsHistoric        :: [HistoricRoundState]
  } deriving (Eq, Show, Generic)
deriveBoth ourAesonOptions ''RoundsState

roundsCurrentLeader :: Getter RoundsState PlayerId
roundsCurrentLeader = field @"roundsLeadershipQueue" . _Wrapped . to NEL.head

roundsCurrentShape :: Lens' RoundsState RoundShape
roundsCurrentShape = field @"roundsCurrent" . field @"currentRoundShape"
roundsCurrentProposal :: Lens' RoundsState ProposalState
roundsCurrentProposal = field @"roundsCurrent" . field @"currentRoundProposal"
roundsCurrentVotes :: Lens' RoundsState [TeamVotingResult]
roundsCurrentVotes = field @"roundsCurrent" . field @"currentRoundVotes"

data GameState
  = WaitingForPlayers PlayerId (Set PlayerId)
  | Pregame (PlayersMap Role) (PlayersMap Bool)
  | Rounds RoundsState
  | FiringRound (PlayersMap Role) [HistoricRoundState]
  | Complete (PlayersMap Role) EndCondition [HistoricRoundState]
  | Aborted PlayerId
  deriving (Eq, Show, Generic)
deriveBoth ourAesonOptions ''GameState

-- These are the events given to us by the UI and the ones stored in the database.
-- We can't just ship these to the UI
data GameStateInputEvent
    = AddPlayer
    | RemovePlayer
    | StartGame
    | ConfirmOk
    | ProposeTeam (Set PlayerId)
    | VoteOnTeam Bool
    | VoteOnProject Bool
    | FirePlayer PlayerId
    | AbortGame
    deriving (Eq, Show, Generic)
deriveBoth ourAesonOptions ''GameStateInputEvent

-- These are the bits where the game decides some kind of random event
data GameStateInternalEvent
  = AssignRoles (PlayersMap Role)
  | ShufflePlayerOrder LeadershipQueue
  deriving (Eq, Show, Generic)
deriveJSON ourAesonOptions ''GameStateInternalEvent

-- These are the events given back to the UI communicating the change that it needs to update.
-- Some of these events contain more information than a single player should get and need to be filtered
-- based on the player before sending them out.
data GameStateOutputEvent
  = PlayerAdded PlayerId
  | PlayerRemoved PlayerId
  | PregameStarted (PlayersMap Role)
  | PlayerConfirmed  -- Lets not say who because the timing could give away info
  | RoundsCommenced RoundsState
  | TeamProposed (Set PlayerId)
  | TeamApproved Natural
  | TeamRejected Natural PlayerId
  | NextRound Bool Natural
  | ThreeSuccessfulProjects
  | PlayerFired PlayerId
  | GameEnded (PlayersMap Role) EndCondition
  | GameAborted PlayerId
  | GameCrashed
  deriving (Eq, Show, Generic)
deriveBoth ourAesonOptions ''GameStateOutputEvent

data GameStateInputError
  = GameIsFull
  | GameOwnerCannotLeave
  | InvalidActionForGameState
  | OwnerMustStartGame
  | NotEnoughPlayers
  | PlayerNotInGame PlayerId
  | GameStateTerminallyInvalid
  | PlayerIsNotLeader
  | PlayerNotInTeam
  | LeaderCannotVoteOnTeam
  | IncorrectTeamSize
  | DuplicateVote
  | PlayerNotManager
  deriving (Eq, Show, Generic)
deriveBoth ourAesonOptions ''GameStateInputError
makeClassyPrisms ''GameStateInputError

cycleLeadershipQueue :: LeadershipQueue -> LeadershipQueue
cycleLeadershipQueue (LeadershipQueue q) = LeadershipQueue . NEL.fromList $ (NEL.tail q) <> [NEL.head q]

calculateScores :: [HistoricRoundState] -> (Natural, Natural)
calculateScores hs = (getSum *** getSum) . foldMap score $ hs^..traverse.field @"historicRoundResult"
  where
    score (RoundSuccess _) = (Sum 1,Sum 0)
    score (RoundFailure _) = (Sum 0,Sum 1)
    score RoundNoConsensus = (Sum 0,Sum 3)

currentToHistoric :: CurrentRoundState -> RoundResult -> HistoricRoundState
currentToHistoric rs = HistoricRoundState
  (rs ^. field @"currentRoundShape")
  (rs ^? field @"currentRoundProposal"._As @"Approved"._1)
  (rs ^. field @"currentRoundVotes")

playersCount :: Int -> Maybe PlayersCount
playersCount = \case
  5  -> Just Players5
  6  -> Just Players6
  7  -> Just Players7
  8  -> Just Players8
  9  -> Just Players9
  10 -> Just Players10
  _ -> Nothing

playersCountToInt :: PlayersCount -> Int
playersCountToInt = \case
  Players5  -> 5
  Players6  -> 6
  Players7  -> 7
  Players8  -> 8
  Players9  -> 9
  Players10 -> 10

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

initialRoundsState :: (PlayersMap Role) -> LeadershipQueue -> PlayersCount -> RoundsState
initialRoundsState roles order = \case
  Players5  -> rss 2 3 2 3 3 False
  Players6  -> rss 2 3 4 3 4 False
  Players7  -> rss 2 3 3 4 4 True
  Players8  -> rss 3 4 4 5 5 True
  Players9  -> rss 3 4 4 5 5 True
  Players10 -> rss 3 4 4 5 5 True
  where
    rss r1 r2 r3 r4 r5 b = RoundsState roles order (CurrentRoundState (rs r1 False) NoProposal [])
      [(rs r2 False),(rs r3 False),(rs r4 b),(rs r5 False)]
      []
    rs n b = RoundShape n b
