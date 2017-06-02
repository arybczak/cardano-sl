{-# LANGUAGE TemplateHaskell #-}

-- | Types describing runtime errors related to Wallet.

module Pos.Wallet.Web.Error.Types
       ( WalletError (..)
       , _InternalError
       , _RequestError
       ) where

import           Universum

import           Control.Lens        (makePrisms)
import qualified Data.Text.Buildable
import           Formatting          (bprint, stext, (%))

data WalletError
    -- | Reasonable error for given request
    -- (e.g. get info about non-existent wallet).
    -- However, this separation is still a bit conditional, may require remake
    = RequestError !Text
    -- | Internal info, which ideally should never happen
    | InternalError !Text
    deriving (Show, Generic)

makePrisms ''WalletError

instance Exception WalletError

instance Buildable WalletError where
    build (RequestError  msg) = bprint ("Request error ("%stext%")") msg
    build (InternalError msg) = bprint ("Internal error ("%stext%")") msg
