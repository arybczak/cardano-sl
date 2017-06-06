{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module containing common parts for all wallet modes.

module Pos.Wallet.Web.Server.Full.Common
       ( nat
       , natS
       , convertHandler
       ) where

import           Universum

import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Except          (MonadError (throwError))
import           Data.Tagged                   (Tagged (..))
import qualified Ether
import           Mockable                      (runProduction)
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import           System.Wlog                   (usingLoggerName)

import           Pos.Block.BListener           (runBListenerStub)
import           Pos.Client.Txp.Balances       (runBalancesRedirect)
import           Pos.Client.Txp.History        (runTxHistoryRedirect)
import           Pos.Communication             (NodeId)
import           Pos.Communication.PeerState   (PeerStateSnapshot, PeerStateTag,
                                                WithPeerState (..), getAllStates,
                                                peerStateFromSnapshot,
                                                runPeerStateRedirect)
import           Pos.Context                   (NodeContext, NodeContextTag)
import           Pos.DB                        (NodeDBs, getNodeDBs, runDBPureRedirect)
import           Pos.DB.Block                  (runBlockDBRedirect)
import           Pos.DB.DB                     (runGStateCoreRedirect)
import           Pos.Delegation.Class          (DelegationVar, askDelegationState)
import           Pos.DHT.Real                  (KademliaDHTInstance)
import           Pos.Discovery                 (askDHTInstance, getPeers,
                                                runDiscoveryConstT, runDiscoveryKademliaT)
import           Pos.Slotting                  (SlottingVar, SomeSlottingSettings,
                                                askSlotting, runSlotsDataRedirect,
                                                runSlotsRedirect)
import           Pos.Ssc.Extra                 (SscMemTag, SscState)
import           Pos.Ssc.Extra.Class           (askSscMem)
import           Pos.Txp                       (GenericTxpLocalData, TxpHolderTag,
                                                askTxpMem)
import           Pos.Wallet.SscType            (WalletSscType)
import           Pos.Wallet.WalletMode         (runBlockchainInfoRedirect,
                                                runUpdatesRedirect)
import           Pos.Wallet.Web.Server.Methods (WalletWebHandler)
import           Pos.Wallet.Web.Server.Sockets (ConnectionsVar, getWalletWebSockets,
                                                runWalletWS)
import           Pos.Wallet.Web.State          (WalletState, getWalletWebState,
                                                runWalletWebDB)
import           Pos.WorkMode                  (RawRealMode (..), RawRealModeK,
                                                RawRealModeS, TxpExtra_TMP)

type WebHandler = WalletWebHandler (RawRealModeK WalletSscType)

type WebHandlerS = WalletWebHandler (RawRealModeS WalletSscType)

-- TODO: eliminate copy-paste

nat :: WebHandler (WebHandler :~> Handler)
nat = do
    ws1         <- getWalletWebState
    tlw1        <- askTxpMem
    ssc1        <- askSscMem
    delWrap1    <- askDelegationState
    psCtx1      <- getAllStates
    nc1         <- Ether.ask @NodeContextTag
    modernDB1   <- getNodeDBs
    conn1       <- getWalletWebSockets
    slotVar1    <- askSlotting
    slotStgs1   <- Ether.ask @SomeSlottingSettings
    kinst1      <- askDHTInstance
    pure $ NT (\h -> convertHandler nc1 modernDB1 tlw1 ssc1 ws1 delWrap1
                              psCtx1 conn1 slotVar1 slotStgs1 (Left (kinst1, h)))

natS :: WebHandlerS (WebHandlerS :~> Handler)
natS = do
    ws         <- getWalletWebState
    tlw        <- askTxpMem
    ssc        <- askSscMem
    delWrap    <- askDelegationState
    psCtx      <- getAllStates
    nc         <- Ether.ask @NodeContextTag
    modernDB   <- getNodeDBs
    conn       <- getWalletWebSockets
    slotVar    <- askSlotting
    slotStgs   <- Ether.ask @SomeSlottingSettings
    peers      <- getPeers
    pure $ NT (\h -> convertHandler nc modernDB tlw ssc ws delWrap
                              psCtx conn slotVar slotStgs (Right (peers, h)))

convertHandler
    :: NodeContext WalletSscType              -- (.. insert monad `m` here ..)
    -> NodeDBs
    -> GenericTxpLocalData TxpExtra_TMP
    -> SscState WalletSscType
    -> WalletState
    -> DelegationVar
    -> PeerStateSnapshot
    -> ConnectionsVar
    -> SlottingVar
    -> SomeSlottingSettings
    -> Either (KademliaDHTInstance, WebHandler a) (Set NodeId, WebHandlerS a)
    -> Handler a
convertHandler nc modernDBs tlw ssc ws delWrap psCtx
               conn slotVar slotSettings handler =
    liftIO (either kRunner sRunner handler) `Catch.catches` excHandlers
  where
    sRunner (peers, wh) = rawRunner . runDiscoveryConstT peers . walletRunner $ wh
    kRunner (ki, wh) = rawRunner . runDiscoveryKademliaT ki . walletRunner $ wh
    walletRunner = runWalletWebDB ws . runWalletWS conn

    rawRunner :: forall t . RawRealMode WalletSscType t -> IO t
    rawRunner (RawRealMode act) = runProduction
           . usingLoggerName "wallet-api"
           . flip Ether.runReadersT nc
           . (\m -> do
               peerStateCtx <- peerStateFromSnapshot psCtx
               Ether.runReadersT m
                   ( Tagged @NodeDBs modernDBs
                   , Tagged @SlottingVar slotVar
                   , Tagged @SomeSlottingSettings slotSettings
                   , Tagged @SscMemTag ssc
                   , Tagged @TxpHolderTag tlw
                   , Tagged @DelegationVar delWrap
                   , Tagged @PeerStateTag peerStateCtx
                   ))
           . runDBPureRedirect
           . runBlockDBRedirect
           . runSlotsDataRedirect
           . runSlotsRedirect
           . runBalancesRedirect
           . runTxHistoryRedirect
           . runPeerStateRedirect
           . runGStateCoreRedirect
           . runUpdatesRedirect
           . runBlockchainInfoRedirect
           . runBListenerStub
           $ act
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
{-# NOINLINE convertHandler #-}
