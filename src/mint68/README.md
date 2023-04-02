# A Minting Policy Implementing CIP 68

*By Antonio Hernández-Garduño*


## About

This project implements a minting policy which explores the basics of CIP 68 (see refrence [1]).

The main objective is to use this example as a guide to construct an efficient work-flow involving on-chain and off-chain code, as well as (in the near future) testing and optimization.

The main takeaway of this exercise is that giving clear *constraint specifications* is key for future smart contract development.


## Introduction

As with any validator, the key for the correct implementation of a Minting Policy is the understanding of the *dialectic* relation between on-chain and off-chain code.

-   Off-chain code *constructs* the desired transactions.
-   On-chain code *destructs* transactions.  What we mean by this figure of speech is that on-chain code is the gatekeeper that will turn away all transactions except for those that satisfy the most stringent validity conditions.

The paramount aspects of a smart contract are *security* and *correctness*.  Thus, of the two listed above, On-chain code is the most important.  This is due to the Blockhain's "code is law" ethos and the fact that, once deployed, it is not possible to change a smart contract.  It is therefore of utmost importance that no illicit transaction be allowed.

The following section on Specifications describes the *requirements* and *constraints* of the minting policy implemented in this project.  The constraints describe various ways in which the minting *should fail*, so they are closely related to on-chain code.


## Specifications

Keep in mind that the specifications for this minting policy constitute just an example.  The actual specifications that will be used in production will of course be different.


### Requirements

A trusted Administrator is responsible of minting an NFT representing a real estate property.  This NFT

-   should be linked to *mutable metadata* that stores the various aspects associated with the property, and
-   be deposited in the owner's wallet address.

Following CIP 68 overall idea, instead of minting one NFT, our minting policy requires the simultaneous minting of a pair of NFTs with the same policy id and named, respectively, "(100)REFERENCE" and "(222)USER".  The USER token is the one deposited in the owner's wallet and the REFERENCE token is deposited at a particular script address which only allows the Administrator to spend its UTxOs (and thus evolve their datums).  The valid datums at this script address are required to have the record structure

```haskell
data RegDatum = RegDatum
  { rdURL  :: BuiltinByteString   -- url to data at IPFS
  , rdHash :: BuiltinByteString   -- hash of data at IPFS
  }
```

for storing an ipfs url and the hash of the metadata.  By evolving the datum associated with the REFERENCE token it is possible to modify the record of the real estate property represented by the NFT.  (The description of this evolution and its associated smart contract is outside the scope of this Readme.)


### Constraints

The minting should fail if any of the following constraints is not satisfied:

-   The pair of USER and REFERENCE NFT's are minted in the same transaction.  No other token is minted.
-   Their names are, respectively, (222)USER and (100)REFERENCE.
-   The USER token is deposited at a public-key-hash address (i.e. a wallet address).
-   The REFERENCE token is deposited at a particular script address determined by the policy parameters (see section 'API implementation' below).  We call this script *the Registry*.
-   The Registry has the property that only the 'Administrator' can spend its contents.  (Thus, only the administrator can evolve the reference datums.)
-   The minting transaction will spend a particular UTxO, also determined by the policy parameters (see below).
-   The REFERENCE token is deposited at the Registry together with a datum.
-   The structure of this datum is necessarily of the form specified above in Requirements.


### Files

This project's code is stored in two directories:

1.  `src`

    -   `Config.hs` - configuration files
    -   `Deploy.hs` - deployment tools
    -   `Minting.hs` - defines minting policy
    -   `Registry.hs` - defines de registry's validator

2.  `app`

    The project's executable is defined in `Main.hs`, which implements the API that gets the policy parameters.


## API implementation

*Objective:*  implement an API that allows to pass the *policy parameters* to a plutus program in order to obtain the seriealized scripts and policy id associated with the minting policy defined in this project.


### Input:  policy parameters

Recall that the minting policy described in this project depends on the following parameters:

-   Reference to the UTxO to be consumed at minting (`TxOutRefId` + `TxOutRefIdx`)
-   Administrator's pub-key-hash

**The policy id is determined by the value of these parameters and the actual logic of the minting policy.**

This project assumes that the *policy parameters* are passed in the file `./ledger/policy-params.json` .  Its structure is illustrated by the example:

```javascript
{
    "utxoRefId": "4a16c4f23a1855392496650deac62eb6c1fa1887f7a2e59ee2a3a3c7b67fa733",
    "utxoRefIdx": 1,
    "pkh": "0e454ff759f367e7fd5b2d0ed30b46b95cf56396af700a3f9971d7a2"
}
```

Note that the pub-key-hash is obtained from the administrator's public key.  With `cardano-cli` this is easily done with the command:

```shell
cardano-cli address key-hash --payment-verification-key-file wallet1.vkey
```

(Here `./ledger/wallet1.vkey` plays the role of the administrator's public key.)


### Output:  two scripts and policy id

Run the command

```shell
$ cabal run write-minting-policy
```

This produces three files, written in directory `./ledger/`, which are:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">File</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">registry.plutus</td>
<td class="org-left">serialized 'Registry Script'</td>
</tr>


<tr>
<td class="org-left">mint.plutus</td>
<td class="org-left">serialized minting policy</td>
</tr>


<tr>
<td class="org-left">mint.policy</td>
<td class="org-left">policy id</td>
</tr>
</tbody>
</table>

Recall that:

-   The *Registry Script* serves the purpose of storing the reference tokens with evolving metadata (as datum)
-   The policy id (synonymous to currency symbol) is the hash of the minting policy


### Other minting pre-requisites

The actual minting is done with an off-chain tool like *cardano-cli*, *Lucid,* *PyCardano* or *Atlas*.  Besides the files `registry.plutus`, `mint.plutus`, and `mint.policy` obtained above, the minting transaction requires some more serialization work.  For completeness, here we describe how the corresponding files are obtained using GHC's REPL and `cardano-cli`.

-   Datum
    
    Recall that the minting policy assumes a datum with the structure described above in section 'Specifications'.  File `./src/Config.hs` contains an example '`datum_1`'.  Its json serialization can be obtained using the REPL.
    
    To access the REPL simply run the command

```shell
$ cabal repl
```

Inside the REPL:

```shell
> import Deploy
> writeJSON "ledger/datum1.json" datum_1 
```

-   Unit
    
    The json representation of 'unit' needs to be present.  It has the expression `{"constructor":0,"fields":[]}` .  You can just copy this expression or generate it with the REPL:

```shell
> writeUnit "ledger/unit.json"
```

-   Token names in hex format
    
    The `cardano-cli` requires to pass the token names in hex format.  There are many tools, even on the web, to do this.  You can use the REPL to easily do this:

```shell
> writeTokenName "ledger/refTokenName.hex" referenceTokenName 
> writeTokenName "ledger/usrTokenName.hex" userTokenName 
```

-   Address of registry script
    
    We obtained the file `registry.plutus` above, but its associated script address is also needed.  This can be easily obtained with `cardano-cli`:

```shell
$ cardano-cli address build \
            --payment-script-file ledger/registry.plutus \
            --testnet-magic 2 \
            --out-file ledger/registry.addr
```


### Minting transaction

For completeness, here we showcase the building of the minting transaction using `cardano-cli`.  Let us assume that `wallet1` is the Administrator's wallet and `wallet2` is the User's (i.e. Owner's) wallet.  The code in shell script `./ledger/mint_txbuild.sh`, which builds the minting transaction, is:

```shell
cardano-cli transaction build \
	    --babbage-era \
	    --testnet-magic 2 \
	    --tx-in $(cat oref1a.tmp) \
	    --required-signer wallet1.skey \
	    --tx-in-collateral $(cat oref1b.tmp) \
	    --tx-out $(cat registry.addr)+"2000000 lovelace"+"1 $(cat mint.policy).$(cat refTokenName.hex)" \
            --tx-out-inline-datum-file datum1.json \
            --tx-out $(cat wallet2.addr)+"2000000 lovelace"+"1 $(cat mint.policy).$(cat usrTokenName.hex)" \
	    --change-address $(cat wallet1.addr) \
	    --mint "1 $(cat mint.policy).$(cat refTokenName.hex)"+"1 $(cat mint.policy).$(cat usrTokenName.hex)" \
	    --mint-script-file mint.plutus \
	    --mint-redeemer-file unit.json \
	    --protocol-params-file protocol-params.json \
	    --out-file tx_mint.body
```

Once the transaction is buiilt, it can be signed and submitted with `./ledger/mint_SignAndSubmit.sh` .


## References

[1]  [CIP 68 - Datum Metadata Standard](https://cips.cardano.org/cips/cip68/) .

