# Toy smart contract:  a tutorial

*By Antonio Hernández-Garduño*


## About

This tutorial describes the deployment of a smart contract (toy example) on the Cardano testnet.

The contract validates when the redeemer, which is an integer number, matches "43".  It is a modification of an example presented by Lars Brünjes at the Plutus Pioneer Program; see reference [1].


## Initial setup


### Cardano node and socket

We assume that you have a Cardano node running on your machine.  To run the `cardano-cli` commands, you first need to export the environment variable binded to the node socket, executing something like:

```shell
export CARDANO_NODE_SOCKET_PATH=$HOME/testnet/db/node.socket
```


### Protocol parameters

At some point (namely, when retrieving funds from a script address) we will need the *protocol parameters* to build a transaction.  We generate the file `protocol-params.json` with:

```shell
[bash]$ cardano-cli query protocol-parameters --testnet-magic 1 --out-file ./protocol-params.json
```


### Wallets

We need two wallets:

-   Wallet 1:  the "giver" that deposits 200 Ada at the script address
-   Wallet 2:  the "grabber" that, provided she guesses correctly, retrieves the funds from the script address

First, we generate the key pair for wallet 1 and wallet 2:

```shell
[bash]$ cardano-cli address key-gen --verification-key-file wallet1.vkey --signing-key-file wallet1.skey
[bash]$ cardano-cli address key-gen --verification-key-file wallet2.vkey --signing-key-file wallet2.skey
```

The two wallets are different.  We can verify this by inspecting both vkeys:

```shell
[bash]$ cat wallet1.vkey
{
    "type": "PaymentVerificationKeyShelley_ed25519",
    "description": "Payment Verification Key",
    "cborHex": "582023b1e22abc0a0369668deb6ce2d5da9acdc3d6dd62b72cb501aebb0472e091d6"
}
[bash]$ cat wallet2.vkey
{
    "type": "PaymentVerificationKeyShelley_ed25519",
    "description": "Payment Verification Key",
    "cborHex": "58201dfbac16291b82056601bc30b1edd45b5a40782027432b81f36a765aed5974c0"
}
```


### Addresses

The addresses associated with wallet 1 and wallet 2 are generated with:

```shell
[bash]$ cardano-cli address build --payment-verification-key-file wallet1.vkey --testnet-magic 1 --out-file wallet1.addr
[bash]$ cardano-cli address build --payment-verification-key-file wallet2.vkey --testnet-magic 1 --out-file wallet2.addr
```

We can inspect, for example, what the address for wallet 1 looks like:

```shell
[bash]$ cat wallet1.addr ; echo
addr_test1vpmyr3ne925nc2hktxm67ulaywr5pak2xtm8kthtz6un96g9pwc8y
```


### Initial funding

We funded  wallet 1 using the [Testnet faucet](https://docs.cardano.org/cardano-testnet/tools/faucet).  We verify that that wallet 1 has been funded with 10,000 Ada:

```shell
[bash]$ cardano-cli query utxo --address $(cat wallet1.addr) --testnet-magic 1
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
af610359200933b7588c4d59f19efa496185bf77ff88895a5b8248e9a86dc9d9     0        10000000000 lovelace + TxOutDatumNone
```

We save the `TxOutRef` of this single UTxO:

```shell
echo -n "af610359200933b7588c4d59f19efa496185bf77ff88895a5b8248e9a86dc9d9#0" > oref1.tmp
```

To fund wallet 2 (with a small amount only needed as collateral), we run the script `fund_wallet2.sh`:

```shell
cardano-cli transaction build \
    --testnet-magic 1 \
    --tx-in $(cat oref1.tmp) \
    --tx-out $(cat wallet2.addr)+"15000000 lovelace" \
    --change-address $(cat wallet1.addr) \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file wallet1.skey \
    --testnet-magic 1 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1 \
    --tx-file tx.signed
```

Execution of the script yields

```shell
[bash]$ ./fund_wallet2.sh
Estimated transaction fee: Lovelace 165721
Transaction successfully submitted.
```

and now wallet 2 should be funded:

```shell
[bash]$ cardano-cli query utxo --address $(cat wallet2.addr) --testnet-magic 1
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
226e76993131b0ca10f7720ecbbd132b7b909e3c82893ef511fe011ed29621b5     0        15000000 lovelace + TxOutDatumNone
```

Let us record this `TxOutRef` on file `oref2.tmp`:

```shell
[bash]$ echo -n "226e76993131b0ca10f7720ecbbd132b7b909e3c82893ef511fe011ed29621b5#0" > oref2.tmp
```

The UTxO at wallet 1 has now changed:

```shell
[bash]$ cardano-cli query utxo --address $(cat wallet1.addr) --testnet-magic 1
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
226e76993131b0ca10f7720ecbbd132b7b909e3c82893ef511fe011ed29621b5     1        9984834279 lovelace + TxOutDatumNone
```

We overwrite `oref1.tmp` with this new UTxO in wallet 1:

```shell
[bash]$ echo -n "226e76993131b0ca10f7720ecbbd132b7b909e3c82893ef511fe011ed29621b5#1" > oref1.tmp
```


## Validator

The validator is written in haskell.  The serialized version is described in next section.


### `ToySC.hs`

```haskell
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module ToySC where

import qualified PlutusTx
import           PlutusTx.Prelude     as TxPrelude hiding (unless)
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger               hiding (singleton)

data ToyRedeemer = ToyRedeemer { key :: Integer }

PlutusTx.unstableMakeIsData ''ToyRedeemer

{-# INLINABLE mkToyValidator #-}
mkToyValidator :: () -> ToyRedeemer -> ScriptContext -> Bool
mkToyValidator _ red _ =
  traceIfFalse "you got it totally wrong!" $ (key red) == 43

data TypedToy
instance Scripts.ValidatorTypes TypedToy where
  type instance DatumType TypedToy = ()
  type instance RedeemerType TypedToy = ToyRedeemer

typedToyValidator :: Scripts.TypedValidator TypedToy
typedToyValidator = Scripts.mkTypedValidator @TypedToy
    $$(PlutusTx.compile [|| mkToyValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator @() @ToyRedeemer

toyValidator :: Validator
toyValidator = Scripts.validatorScript typedToyValidator
```


## Serialization


### Serialization of validator

The command  `writeValidator` ,  defined in module `Deploy`, serializes the  `toyValidator :: Validator`  that we programmed in Haskell.  

```shell
[ghci]$ ToySC> import Deploy

[ghci]$ Deploy ToySC> :t writeValidator 
writeValidator
  :: FilePath
     -> Plutus.V1.Ledger.Scripts.Validator
     -> IO
          (Either (cardano-api-1.35.3:Cardano.Api.Error.FileError ()) ())

[ghci]$ Deploy ToySC> writeValidator "blockchain/toy.plutus" toyValidator
Right ()
```

The serialized validator is written in file `toy.plutus`.


### Serialization of datum

We will also need the serialized Unit:

```shell
[ghci]$ Deploy ToySC> :t writeUnit 
writeUnit :: FilePath -> IO ()
[ghci]$ Deploy ToySC> writeUnit "blockchain/unit.json"
```

which yields:

```shell
[bash]$ cat unit.json ; echo
{"constructor":0,"fields":[]}
```


### Serialization of redeemer

Let us serialize a couple of instances of  `ToyRedeemer { key :: Integer }` , corresponding to '`key = 43`' (valid key) and '`key = 27`' (invalid key).

```shell
[ghci]$ Deploy ToySC> :t writeJSON 
writeJSON
  :: PlutusTx.IsData.Class.ToData a => FilePath -> a -> IO ()

[ghci]$ Deploy ToySC> writeJSON "blockchain/fortyThree.json" $ ToyRedeemer 43

[ghci]$ Deploy ToySC> writeJSON "blockchain/twentySeven.json" $ ToyRedeemer 27
```

Lock funds on the script

First wallet 1 sends 200 Ada to the script address.


## Send funds to script


### Script address

To lock funds at this script we need its script address, which we obtain executing:

```shell
cardano-cli address build \
            --payment-script-file toy.plutus \
            --testnet-magic 1 \
            --out-file toy.addr
```

which generated:

```shell
[bash]$ cat toy.addr ; echo
addr_test1wqux5cqau79klfmyufhqmjuhjn0au6w9y03v2zkq725zy6s29pus6
```


### Send funds to script

We desire to lock 200 Ada at the script address of the `toyValidator`.  This is achieved with the shell script:

This is achieved with shell script  `give.sh`  :

```shell
cardano-cli transaction build \
	    --testnet-magic 1 \
	    --tx-in $(cat oref1.tmp) \
	    --tx-out $(cat toy.addr)+"200000000 lovelace" \
	    --tx-out-datum-hash-file unit.json \
	    --change-address $(cat wallet1.addr) \
	    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file wallet1.skey \
    --testnet-magic 1 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1 \
    --tx-file tx.signed
```

(Parameter `oref1.tmp` was written at the end of section 'Initial setup'.)

Execution of the script succesfully submits the transaction:

```shell
[bash]$ ./give.sh
Estimated transaction fee: Lovelace 167349
Transaction successfully submitted.
```

Therefore the funds at wallet 1 have diminished:

```shell
[bash]$ cardano-cli query utxo --address $(cat wallet1.addr) --testnet-magic 1
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2ac700f7caf6bd378f20900ed6e269e71296c544373262fcb9ed5e7f79d7751d     1        9784666930 lovelace + TxOutDatumNone
```

and the script address now has 200 Ada:

```shell
[bash]$ cardano-cli query utxo --address $(cat toy.addr) --testnet-magic 1
TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2ac700f7caf6bd378f20900ed6e269e71296c544373262fcb9ed5e7f79d7751d     0        200000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
```

Note that the `TxOutDatumHash` is indeed the hash of unit:

```shell
[bash]$ cardano-cli transaction hash-script-data --script-data-file unit.json
923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec
```

We manually record the `TxOutRef` at the script address in file `orefSc.tmp` :

```shell
[bash]$ cat orefSc.tmp ; echo
2ac700f7caf6bd378f20900ed6e269e71296c544373262fcb9ed5e7f79d7751d#0
```


## Grab

We now want to grab the funds locked at the script with the correct redeemer.

Recall that we have serialized two instances of `ToyRedeemer` :

```shell
[bash]$ cat fortyThree.json ; echo
{"constructor":0,"fields":[{"int":43}]}
[bash]$ cat twentySeven.json ; echo
{"constructor":0,"fields":[{"int":27}]}
```


### Grabbing the funds

Shell script `grab.sh` attempts to grab the funds locked at the script:

```shell
cardano-cli transaction build \
            --babbage-era \
            --testnet-magic 1 \
            --tx-in $(cat orefSc.tmp) \
            --tx-in-script-file toy.plutus \
            --tx-in-datum-file unit.json \
            --tx-in-redeemer-file fortyThree.json \
            --tx-in-collateral $(cat oref2.tmp) \
            --change-address $(cat wallet2.addr) \
            --protocol-params-file protocol-params.json \
            --out-file tx.body

cardano-cli transaction sign \
            --tx-body-file tx.body \
            --signing-key-file wallet2.skey \
            --testnet-magic 1 \
            --out-file tx.signed

cardano-cli transaction submit \
            --testnet-magic 1 \
            --tx-file tx.signed
```

Since the redeemer is correct, we do get the funds:

```shell
[bash]$ cardano-cli query utxo --address $(cat wallet2.addr) --testnet-magic 1
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
226e76993131b0ca10f7720ecbbd132b7b909e3c82893ef511fe011ed29621b5     0        15000000 lovelace + TxOutDatumNone
a150416f2571561e34c2c1573835a073193d3d2e6853642f1cd8824a6e15bddd     0        199712616 lovelace + TxOutDatumNone
```


### Trying to grab funds with incorrect redeemer

1.  Fund the script again

    We fund the script again with 200 Ada from wallet 1:
    
    ```shell
    [bash]$ cardano-cli query utxo --address $(cat wallet1.addr) --testnet-magic 1
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    682e1925a5e840592486b7222a67b85c075430ac57930f5ff7d808eacba7f6a9     1        9584499581 lovelace + TxOutDatumNone
    
    [bash]$ echo -n "682e1925a5e840592486b7222a67b85c075430ac57930f5ff7d808eacba7f6a9#1" > oref1.tmp
    
    [bash]$ ./give.sh
    Estimated transaction fee: Lovelace 167349
    Transaction successfully submitted.
    ```

2.  Trying to grab (should fail)

    Choose UTxO to spend:
    
    ```shell
    [bash]$ cardano-cli query utxo --address $(cat toy.addr) --testnet-magic 1                                                                                                     
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    c767d36b09e1c478bc61cb70a2480ac0a7155a8f16e041cc73b04a41d85560f4     0        200000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
    [bash]$ echo -n "c767d36b09e1c478bc61cb70a2480ac0a7155a8f16e041cc73b04a41d85560f4#0" > orefSc.tmp
     ```
     
    Try to grab:
    
    ```shell
    [bash]$ cardano-cli transaction build \
    >             --babbage-era \
    >     --testnet-magic 1 \
    >     --tx-in $(cat orefSc.tmp) \
    >     --tx-in-script-file toy.plutus \
    >     --tx-in-datum-file unit.json \
    >     --tx-in-redeemer-file twentySeven.json \
    >     --tx-in-collateral $(cat oref2.tmp) \
    >     --change-address $(cat wallet2.addr) \
    >     --protocol-params-file protocol-params.json \
    >     --out-file tx.body
    Command failed: transaction build  Error: The following scripts have execution failures:
    the script for transaction input 0 (in the order of the TxIds) failed with: 
    The Plutus script evaluation failed: An error has occurred:  User error:
    The machine terminated because of an error, either from a built-in function or from an explicit use of 'error'.
    Script debugging logs: you got it totally wrong!
    PT5
    ```
    
    Note that we did not even bother to try to submit the transaction, since we already know it fails.


## References

[1]  [Plutus Pioneer Program - Iteration #3 - Lecture #2](https://www.youtube.com/playlist?list=PLNEK_Ejlx3x0mhPmOjPSHZPtTFpfJo3Nd) .

