{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}

module Spec.Trace where

import Control.Monad          (void)
import Control.Lens
import Data.Default           (Default (..))
import qualified Data.Map                   as Map
import           Ledger.Value
import           Ledger.Ada                 as Ada
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (not)
import           Prelude                    (IO)
import           Plutus.Contract.Test
import           Test.Tasty
-- import qualified Test.Tasty.HUnit as HUnit


import OffChain


tests :: TestTree
tests = testGroup "UnitTests"
  [ checkPredicateOptions
      myOptions
      "incorrect key"
      myPredicate1
      trace1
  , checkPredicateOptions
      myOptions
      "correct key"
      myPredicate2
      trace2
  ]

myOptions :: CheckOptions
myOptions = defaultCheckOptions & emulatorConfig .~ emCfg

myPredicate1 :: TracePredicate
myPredicate1 = not assertNoFailedTransactions

myPredicate2 :: TracePredicate
myPredicate2 = assertNoFailedTransactions

test :: IO ()
test = runEmulatorTraceIO' def emCfg trace1


-- INITIALIZATION --

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
  callEndpoint @"grab" h2 42

  void $ Emulator.waitNSlots 1

trace2 :: EmulatorTrace ()
trace2 = do
  h1 <- activateContractWallet w1 endpoints
  h2 <- activateContractWallet w2 endpoints

  -- | Wallet 1 deposits to script
  callEndpoint @"give" h1 200_000_000

  void $ Emulator.waitNSlots 1

  -- | Wallet 2 submits her "guess" and tries to grab from script
  callEndpoint @"grab" h2 43

  void $ Emulator.waitNSlots 1
