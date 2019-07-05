{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, LambdaCase, OverloadedStrings, TypeApplications #-}
module Game.Dissidence.GameState where

-- See https://www.ultraboardgames.com/avalon/game-rules.php for deets

import Control.Lens

import           Control.Arrow         ((***))
import           Control.Monad.Except  (MonadError, throwError)
import           Control.Monad.Trans   (lift)
import           Data.Bool             (bool)
import           Data.Foldable         (and)
import           Data.Generics.Product (field)
import           Data.Generics.Sum     (_As)
import           Data.List.NonEmpty    (NonEmpty)
import qualified Data.List.NonEmpty    as NEL
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Monoid           (Sum (Sum), getSum)
import           Data.Random.List      (randomElement, shuffle)
import           Data.RVar             (MonadRandom, sampleRVar)
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import           Numeric.Natural       (Natural)

newtype PlayerId = PlayerId { unPlayerId :: Natural } deriving (Eq, Ord, Show, Generic)

type RolesMap = Map PlayerId (Player, Role)

data Player = Player
  { playerId   :: PlayerId
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
  | Pregame (Map PlayerId (Player,Role,Bool))
  | Rounds RoundsState
  | FiringRound RolesMap [HistoricRoundState]
  | Complete RolesMap EndCondition [HistoricRoundState]
  | Aborted PlayerId
  deriving (Eq, Show, Generic)

data RoundsState = RoundsState
  { roundsRoles           :: RolesMap
  , roundsLeadershipQueue :: NonEmpty PlayerId
  , roundsCurrent         :: CurrentRoundState
  , roundsFuture          :: [RoundShape]
  , roundsHistoric        :: [HistoricRoundState]
  } deriving (Eq, Show, Generic)

roundsCurrentLeader :: Getter RoundsState PlayerId
roundsCurrentLeader = field @"roundsLeadershipQueue" . to NEL.head

roundsCurrentShape :: Lens' RoundsState RoundShape
roundsCurrentShape = field @"roundsCurrent" . field @"currentRoundShape"
roundsCurrentProposal :: Lens' RoundsState ProposalState
roundsCurrentProposal = field @"roundsCurrent" . field @"currentRoundProposal"
roundsCurrentVotes :: Lens' RoundsState [TeamVotingResult]
roundsCurrentVotes = field @"roundsCurrent" . field @"currentRoundVotes"

data CurrentRoundState = CurrentRoundState
 { currentRoundShape    :: RoundShape
 , currentRoundProposal :: ProposalState
 , currentRoundVotes    :: [TeamVotingResult]
 } deriving (Eq,Show, Generic)

data ProposalState
  = NoProposal
  | Proposed (Set PlayerId) (Map PlayerId Bool)
  | Approved (Set PlayerId) (Map PlayerId Bool)
  deriving (Eq, Show, Generic)

data TeamVotingResult = TeamVotingResult
  { votingTeamLeader :: PlayerId
  , votingResultTeam :: Set PlayerId
  , votingResult     :: Map PlayerId Bool
  } deriving (Eq, Show, Generic)

data RoundResult = RoundSuccess Natural | RoundFailure Natural | RoundNoConsensus deriving (Eq, Show, Generic)

data HistoricRoundState = HistoricRoundState
  { historicRoundShape  :: RoundShape
  , historicRoundTeam   :: Maybe (Set PlayerId)
  , historicRoundVotes  :: [TeamVotingResult]
  , historicRoundResult :: RoundResult
  } deriving (Eq, Show, Generic)

data RoundShape = RoundShape
  { roundShapeTeamSize :: Natural
  , roundShapeTwoFails :: Bool
  } deriving (Eq, Show, Generic)

-- These are the events given to us by the UI and the ones stored in the database.
-- We can't just ship these to the UI
data GameStateInputEvent
    = AddPlayer Player
    | RemovePlayer PlayerId
    | StartGame PlayerId
    | ConfirmOk PlayerId
    | ProposeTeam PlayerId (Set PlayerId)
    | VoteOnTeam PlayerId Bool
    | VoteOnProject PlayerId Bool
    | FirePlayer PlayerId
    | AbortGame PlayerId
    deriving (Eq, Show, Generic)

-- These are the bits where the game decides some kind of random event
data GameStateInternalEvent
  = AssignRoles (Map PlayerId Role)
  | ShufflePlayerOrder (NonEmpty PlayerId)
  deriving (Eq, Show, Generic)

-- These are the events given back to the UI communicating the change that it needs to update.
-- Some of these events contain more information than a single player should get and need to be filtered
-- based on the player before sending them out.
data GameStateOutputEvent
  = PlayerAdded Player
  | PlayerRemoved PlayerId
  | PregameStarted (Map PlayerId Role)
  | PlayerConfirmed  -- Lets not say who because the timing could give away info
  | RoundsCommenced RoundsState
  | TeamProposed (Set PlayerId)
  | TeamApproved Natural
  | TeamRejected Natural PlayerId
  | NextRound Bool Natural
  | ThreeSuccessfulProjects
  | PlayerFired PlayerId
  | GameEnded RolesMap EndCondition
  | GameAborted PlayerId
  | GameCrashed
  deriving (Eq, Show, Generic)

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
  deriving (Eq, Show, Generic)

-- We have some non referentially transparent reactions to input events, so we need to
-- store and replay a little more to recreate the view of the world. The idea is that
-- the GameStateInternalEvents get serialised
inputEvent
  :: (MonadError GameStateInputError m, MonadRandom m)
  => GameState
  -> GameStateInputEvent
  -> Maybe GameStateInternalEvent
  -> m (GameState, Maybe GameStateInternalEvent, Maybe GameStateOutputEvent)
inputEvent gs ev iEvMay = case gs of
  WaitingForPlayers o ps -> case ev of
    AddPlayer p@(Player pId pName) ->
      if Map.member pId ps || (o ^. field @"playerId") == pId then pure (gs, Nothing, Nothing)
      else if Map.size ps >= 9 then throwError GameIsFull
      else pure (WaitingForPlayers o (Map.insert pId p ps), Nothing, Just $ PlayerAdded p)

    RemovePlayer  pId ->
      if (o ^. field @"playerId") == pId then throwError GameOwnerCannotLeave
      else pure (WaitingForPlayers o (Map.delete pId ps), Nothing, Just $ PlayerRemoved pId)

    StartGame pId ->
      if (o ^. field @"playerId") /= pId then throwError OwnerMustStartGame
      else
        let allPlayersMap = Map.insert (o^. field @"playerId") o ps
        in case (playersCount . Map.size $ allPlayersMap) of
          Nothing -> throwError NotEnoughPlayers
          Just pc -> do
            rolesMap <- case iEvMay of
              Just (AssignRoles rolesMap) -> pure rolesMap
              _ -> do
                roles <- sampleRVar . shuffle $ playersToRoles pc
                pure . Map.fromList $ zipWith
                  (\p@(Player pId _) r -> (pId,r))
                  (Map.elems allPlayersMap)
                  roles

            let rolesPlayerMap = Map.intersectionWith
                  (\p r -> (p,r,False))
                  allPlayersMap
                  rolesMap
            pure
              ( Pregame rolesPlayerMap
              , Just $ AssignRoles rolesMap
              , Just $ PregameStarted rolesMap
              )

    AbortGame pId -> abortGame pId
    _ -> invalidAction

  Pregame roleConfirms -> case ev of
    ConfirmOk pId ->
      if not (Map.member pId roleConfirms) then throwError $ PlayerNotInGame pId
      else
        let newConfirms = roleConfirms & ix pId . _3 .~ True
        in
          if not (and ((^. _3) <$> newConfirms))
          then pure (Pregame newConfirms, Nothing, Just PlayerConfirmed)
          else case (playersCount (Map.size newConfirms)) of
            Nothing   -> throwError GameStateTerminallyInvalid
            (Just pc) -> do
              order <- case iEvMay of
                (Just (ShufflePlayerOrder nel)) ->
                  if (NEL.toList nel) == (Map.keys roleConfirms)
                  then pure nel
                  else throwError GameStateTerminallyInvalid
                _ ->
                  NEL.fromList <$> sampleRVar (shuffle $ Map.keys newConfirms)

              let roles = (\(p,r,_) -> (p,r)) <$> newConfirms
              let roundsState = initialRoundsState roles order pc
              pure
                ( Rounds roundsState
                , Just $ ShufflePlayerOrder order
                , Just $ RoundsCommenced roundsState
                )

    AbortGame pId -> abortGame pId
    _ -> invalidAction

  Rounds rs -> case (rs ^.roundsCurrentProposal) of
    NoProposal -> case ev of
      ProposeTeam pId ps ->
        if pId /= (rs ^.roundsCurrentLeader) then throwError PlayerIsNotLeader
        else if (Set.size ps) /= rs ^.roundsCurrentShape.field @"roundShapeTeamSize".to fromIntegral
          then throwError IncorrectTeamSize
          else pure
            ( Rounds (rs & roundsCurrentProposal .~ Proposed ps Map.empty)
            , Nothing
            , Just $ TeamProposed ps
            )
      AbortGame pId -> abortGame pId
      _ -> invalidAction

    Proposed ps votes -> case ev of
      VoteOnTeam pId pass ->
        let teamSansLeader = Map.delete (rs ^.roundsCurrentLeader) (rs ^. field @"roundsRoles")
            newVotes = Map.insert pId pass votes
            passVotes = fromIntegral . length . filter id . Map.elems $ newVotes
            failVotes = fromIntegral . length . filter not . Map.elems $ newVotes
        in
          if pId == rs ^.roundsCurrentLeader then throwError LeaderCannotVoteOnTeam
          else if not (Map.member pId teamSansLeader) then throwError $ PlayerNotInGame pId
          else if (Map.member pId votes) then throwError DuplicateVote
          else if (Map.size newVotes < Map.size teamSansLeader)
            then pure (Rounds (rs & roundsCurrentProposal .~ Proposed ps newVotes), Nothing, Nothing)
            else
              let newVoteHistory = TeamVotingResult (rs^.roundsCurrentLeader) ps newVotes
              in
                if (passVotes > failVotes)
                then pure
                  ( Rounds $ rs
                    & roundsCurrentProposal .~ Approved ps Map.empty
                    & roundsCurrentVotes <>~ [newVoteHistory]
                  , Nothing
                  , Just $ TeamApproved passVotes
                  )
                else if (length (rs^.roundsCurrentVotes)) >= 4
                  then
                    let ec = SideEffectsWin FiveTeamsVetoed
                        cRound = (rs ^. field @"roundsCurrent") & field @"currentRoundVotes" <>~ [newVoteHistory]
                        roundHist = currentToHistoric cRound RoundNoConsensus
                        roundHists = (rs ^. field @"roundsHistoric") <> [roundHist]
                        roles = rs ^. field @"roundsRoles"
                    in pure (Complete roles ec roundHists, Nothing, Just $ GameEnded roles ec)
                  else do
                    let newQueue = cycleLeadershipQueue (rs ^. field @"roundsLeadershipQueue")
                    pure ( Rounds $ rs
                      & roundsCurrentProposal .~ NoProposal
                      & field @"roundsLeadershipQueue" .~ newQueue
                      & roundsCurrentVotes <>~ [newVoteHistory]
                      , Nothing
                      , Just $ TeamRejected passVotes (NEL.head newQueue) -- TODO: This is iffy that we don't send the whole queue
                      )

      AbortGame pId -> abortGame pId
      _ -> invalidAction

    Approved ps votes -> case ev of
      VoteOnProject pId pass ->
        if not (Set.member pId ps) then throwError PlayerNotInTeam
        else if Map.member pId votes then throwError DuplicateVote
        else
          let newVotes    = Map.insert pId pass votes
              fails       = fromIntegral . length . filter not . Map.elems $ newVotes
              failsNeeded = rs ^. roundsCurrentShape . field @"roundShapeTwoFails" . to (bool 1 2)
              newCr       = (rs ^. field @"roundsCurrent") & field @"currentRoundProposal" .~ Approved ps newVotes
          in
            if Map.size newVotes < Set.size ps
            then pure (Rounds $ rs & field @"roundsCurrent" .~ newCr, Nothing, Nothing)
            else
              let failed = fails >= failsNeeded
                  histRound  = currentToHistoric newCr (bool (RoundSuccess fails) (RoundFailure fails) failed)
                  histRounds = (rs ^. field @"roundsHistoric") <> [histRound]
                  (cw,sw)  = calculateScores histRounds
                  roles    = rs ^. field @"roundsRoles"
              in
                if cw >= 3 then
                  pure (FiringRound roles histRounds, Nothing, Just $ ThreeSuccessfulProjects)
                else if sw >= 3 then
                  let wc = SideEffectsWin ThreeFailedProjects
                  in pure (Complete roles wc histRounds, Nothing, Just $ GameEnded roles wc)
                else
                  let nextShape = rs ^? field @"roundsFuture"._head
                  in case nextShape of
                    Nothing -> throwError GameStateTerminallyInvalid
                    Just s  -> pure
                      ( Rounds $ rs
                        & field @"roundsHistoric" .~ histRounds
                        & field @"roundsCurrent" .~ (CurrentRoundState s NoProposal [])
                        & field @"roundsLeadershipQueue" %~ cycleLeadershipQueue
                      , Nothing
                      , Just $ NextRound (not failed) fails
                      )

      AbortGame pId          -> abortGame pId
      _                      -> invalidAction

  FiringRound rs hist -> case ev of
    AbortGame pId -> abortGame pId
    _             -> invalidAction

  Complete _ _ _ -> invalidAction
  Aborted _ -> invalidAction

  where
    abortGame pId = pure (Aborted pId, Nothing, Just $ GameAborted pId)
    invalidAction = throwError InvalidActionForGameState

cycleLeadershipQueue :: NEL.NonEmpty PlayerId -> NEL.NonEmpty PlayerId
cycleLeadershipQueue q = NEL.fromList $ (NEL.tail q) <> [NEL.head q]

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

initialRoundsState :: RolesMap -> NonEmpty PlayerId -> PlayersCount -> RoundsState
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
