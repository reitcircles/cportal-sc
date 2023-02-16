{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}

module Test where

import Control.Monad          (void)
import Data.Default           (Default (..))
import qualified Data.Map                   as Map
import           Ledger.Value
import           Ledger.Ada                 as Ada
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO)
import Wallet.Emulator.Wallet


import OffChain


-- EMULATOR TEST --

test :: IO ()
test = runEmulatorTraceIO' def emCfg trace1


-- INITIALIZATION --

w1, w2 :: Wallet
w1 = knownWallet 1 -- Giver
w2 = knownWallet 2 -- Grabber

v1, v2 :: Value  -- Wallet's initial values
v1 = Ada.lovelaceValueOf 10_000_000_000
v2 = Ada.lovelaceValueOf     15_000_000

emCfg :: EmulatorConfig
emCfg = EmulatorConfig
  { _initialChainState = Left $ Map.fromList
       [ (w1, v1), (w2, v2) ]
  , _params = def
  }

-- TRACES --

trace1 :: EmulatorTrace ()
trace1 = do
  h1 <- activateContractWallet w1 endpoints
  h2 <- activateContractWallet w2 endpoints

  -- | Wallet 1 deposits to script
  callEndpoint @"give" h1 200_000_000

  void $ Emulator.waitNSlots 1

  -- | Wallet 2 submits her "guess" and tries to grab from script
  callEndpoint @"grab" h2 41

  void $ Emulator.waitNSlots 1
