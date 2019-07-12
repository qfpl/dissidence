{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses, OverloadedStrings, TypeApplications, TypeFamilies      #-}
module Game.Dissidence.GameState
  ( inputEvent
  , newGame
  , GameState
  , EndCondition(..)
  , PlayerId(..)
  , RoundsState(..)
  , RoundResult(..)
  , ProposalState(..)
  , RoundShape(..)
  , SideEffectWinCondition(..)
  , LeadershipQueue(..)
  , Role(..)
  , SideEffectRole(..)
  , CrusaderRole(..)
  , HistoricRoundState(..)
  , CurrentRoundState(..)
  , TeamVotingResult(..)
  ) where

-- See https://www.ultraboardgames.com/avalon/game-rules.php for deets

import Control.Lens

import           Control.Monad.Except  (MonadError, throwError)
import           Data.Bool             (bool)
import           Data.Generics.Product (field)
import qualified Data.List.NonEmpty    as NEL
import qualified Data.Map              as Map
import           Data.Random.List      (shuffle)
import           Data.RVar             (MonadRandom, sampleRVar)
import qualified Data.Set              as Set

import Game.Dissidence.GameState.Internal


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
    AddPlayer pId ->
      if Set.member pId ps || o == pId then pure (gs, Nothing, Nothing)
      else if Set.size ps >= 9 then throwError GameIsFull
      else pure (WaitingForPlayers o (Set.insert pId ps), Nothing, Just $ PlayerAdded pId)

    RemovePlayer  pId ->
      if o == pId then throwError GameOwnerCannotLeave
      else pure (WaitingForPlayers o (Set.delete pId ps), Nothing, Just $ PlayerRemoved pId)

    StartGame pId ->
      if o /= pId then throwError OwnerMustStartGame
      else
        let allPlayersSet = Set.insert o ps
        in case (playersCount . Set.size $ allPlayersSet) of
          Nothing -> throwError NotEnoughPlayers
          Just pc -> do
            rolesMap <- case iEvMay of
              Just (AssignRoles rolesMap) -> pure rolesMap
              _ -> do
                roles <- sampleRVar . shuffle $ playersToRoles pc
                pure . Map.fromList $ zip
                  (Set.toList allPlayersSet)
                  roles

            pure
              ( Pregame rolesMap Map.empty
              , Just $ AssignRoles rolesMap
              , Just $ PregameStarted rolesMap
              )

    AbortGame pId -> abortGame pId
    _ -> invalidAction

  Pregame roles confirms -> case ev of
    ConfirmOk pId ->
      if not (Map.member pId roles) then throwError $ PlayerNotInGame pId
      else
        let newConfirms = Map.insert pId True confirms
        in
          if (Map.size roles > length (filter id (Map.elems newConfirms)))
          then pure (Pregame roles newConfirms, Nothing, Just PlayerConfirmed)
          else case (playersCount (Map.size newConfirms)) of
            Nothing   -> throwError GameStateTerminallyInvalid
            (Just pc) -> do
              order <- case iEvMay of
                (Just (ShufflePlayerOrder lq@(LeadershipQueue nel))) ->
                  if (NEL.toList nel) == (Map.keys confirms)
                  then pure lq
                  else throwError GameStateTerminallyInvalid
                _ ->
                  LeadershipQueue . NEL.fromList <$> sampleRVar (shuffle $ Map.keys newConfirms)

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
                      , Just $ TeamRejected passVotes (NEL.head . unLeadershipQueue $ newQueue) -- TODO: This is iffy that we don't send the whole queue
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

  FiringRound roles hist -> case ev of
    FirePlayer pId targetId ->
      if (roles ^? ix pId) /= Just (SneakySideEffects (Just MiddleManager))
        then throwError PlayerNotManager
      else if (roles ^? ix targetId) /= Just (CompositionalCrusaders (Just FPExpert))
        then pure (Complete roles CrusadersWin hist, Nothing, Just $ GameEnded roles CrusadersWin)
      else pure
        ( Complete roles (SideEffectsWin FPExpertFired) hist
        , Nothing
        , Just $ GameEnded roles (SideEffectsWin FPExpertFired)
        )

    AbortGame pId           -> abortGame pId
    _                       -> invalidAction

  Complete _ _ _ -> invalidAction
  Aborted _ -> invalidAction

  where
    abortGame pId = pure (Aborted pId, Nothing, Just $ GameAborted pId)
    invalidAction = throwError InvalidActionForGameState

newGame :: PlayerId -> GameState
newGame owner = WaitingForPlayers owner Set.empty
