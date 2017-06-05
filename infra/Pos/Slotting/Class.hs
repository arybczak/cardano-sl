{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Type class used for slotting functionality.

module Pos.Slotting.Class
       ( SlottingSettings (..)
       , SlottingConstraint
       , SomeSlottingSettings (..)
       , MonadSlots (..)

       , SlotsRedirect
       , runSlotsRedirect
       ) where

import           Universum

import           Control.Monad.Trans          (MonadTrans)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Ether
import           Mockable                     (Catch, CurrentTime, Delay, Fork, Mockables,
                                               Throw)
import           System.Wlog                  (WithLogger)

import           Pos.Core.Types               (SlotId (..), Timestamp)
import           Pos.Slotting.MemState        (MonadSlotsData)
import           Pos.Util.Util                ()

----------------------------------------------------------------------------
-- Settings
----------------------------------------------------------------------------

-- | These settings contain necessary functions to get information
-- about current slot and other slotting-related data.
data SlottingSettings m = SlottingSettings
    {
    -- | Get current slot if it's known, 'Nothing' otherwise.  Slot
    -- may be unknown, because it depends on GState (which may be
    -- outdated) or because of problems with obtaining time.
      ssGetCurrentSlot           :: m (Maybe SlotId)
    -- | Blocking version of 'getCurrentSlot'. This function doesn't
    -- return until current slot is known.
    , ssGetCurrentSlotBlocking   :: m SlotId
    -- | This function tries to predict current slot as accurately as
    -- it can.  If 'getCurrentTime' returns unreliable time, then
    -- function returns last known slot If our slotting data into DB
    -- is outdated, then function tries to extrapolate slot using last
    -- know slotting data
    , ssGetCurrentSlotInaccurate :: m SlotId
    -- | Get current time according as used by slotting. Usually you
    -- don't need this function.
    , ssCurrentTimeSlotting      :: m Timestamp
    }

----------------------------------------------------------------------------
-- Quantification
----------------------------------------------------------------------------

-- | Set of constraints necessary for slotting.
type SlottingConstraint m =
    ( MonadIO m
    , WithLogger m
    , MonadSlotsData m
    , Mockables m
        [ Delay
        , CurrentTime
        ]
    )

data SomeSlottingSettings =
    SomeSlottingSettings (forall m. SlottingConstraint m =>
                                        (SlottingSettings m))

----------------------------------------------------------------------------
-- Type class
----------------------------------------------------------------------------

class MonadSlotsData m => MonadSlots m where
    getCurrentSlot :: m (Maybe SlotId)
    getCurrentSlotBlocking :: m SlotId
    getCurrentSlotInaccurate :: m SlotId
    currentTimeSlotting :: m Timestamp

    default getCurrentSlot :: (MonadTrans t, MonadSlots m', t m' ~ m) =>
        m (Maybe SlotId)
    getCurrentSlot = lift getCurrentSlot

    default getCurrentSlotBlocking :: (MonadTrans t, MonadSlots m', t m' ~ m) =>
        m SlotId
    getCurrentSlotBlocking = lift getCurrentSlotBlocking

    default getCurrentSlotInaccurate :: (MonadTrans t, MonadSlots m', t m' ~ m) =>
        m SlotId
    getCurrentSlotInaccurate = lift getCurrentSlotInaccurate

    default currentTimeSlotting :: (MonadTrans t, MonadSlots m', t m' ~ m) =>
        m Timestamp
    currentTimeSlotting = lift currentTimeSlotting

instance {-# OVERLAPPABLE #-}
    (MonadSlots m, MonadTrans t, Monad (t m)) =>
        MonadSlots (t m)

----------------------------------------------------------------------------
-- Redirect
----------------------------------------------------------------------------

data SlotsRedirectTag

type SlotsRedirect =
    Ether.TaggedTrans SlotsRedirectTag IdentityT

runSlotsRedirect :: SlotsRedirect m a -> m a
runSlotsRedirect = coerce

instance ( SlottingConstraint m
         , Ether.MonadReader' SomeSlottingSettings m
         , t ~ IdentityT
         ) =>
         MonadSlots (Ether.TaggedTrans SlotsRedirectTag t m) where
    getCurrentSlot = do
        SomeSlottingSettings settings <- Ether.ask'
        ssGetCurrentSlot settings

    getCurrentSlotBlocking = do
        SomeSlottingSettings settings <- Ether.ask'
        ssGetCurrentSlotBlocking settings

    getCurrentSlotInaccurate = do
        SomeSlottingSettings settings <- Ether.ask'
        ssGetCurrentSlotInaccurate settings

    currentTimeSlotting = do
        SomeSlottingSettings settings <- Ether.ask'
        ssCurrentTimeSlotting settings
