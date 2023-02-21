# A Minting Policy Implementing CIP 68

*By Antonio Hernández-Garduño*


## About

This project implements a minting policy which explores an basic implementation of CIP 68 (see refrence [1]).

The main objective is to use this example as a guide to construct an efficient work-flow involving on-chain and off-chain code, as well as (in the near future) testing and optimization.


## Introduction

As with any validator, the key for the correct implementation of a Minting Policy is the understanding of the *dialectic* relation between on-chain and off-chain code.

-   Off-chain code *constructs* the desired transactions.
-   On-chain code *destructs* transactions.  What we mean by this figure of speech is that on-chain code is the gatekeeper that will turn away all transactions except for those that satisfy the most stringent validity conditions.

Of the two, On-chain code is the most important.  This is because the paramount aspects of a smart contract are *security* and *correctness*.  This is due to the Blockhain's "code is law" ethos and the fact that, once deployed, it is not possible to change a smart contract.  It is therefore of utmost importance that no ilicit transaction be allowed.

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
  { rdURL  :: BuiltinByteString
  , rdHash :: BuiltinByteString
  }
```

for storing an ipfs url and the hash of the metadata.  By evolving the datum associated with the REFERENCE token it is possible to modify the record of the real estate property represented by the NFT.  (The description of this evolution and its associated smart contract is outside the scope of this Readme.)


### Constraints

The minting should fail if any of the following constraints is not satisfied.

-   The pair of USER and REFERENCE NFT's are minted in the same transaction.  No other token is minted.
-   Their names are, respectively, (222)USER and (100)REFERENCE.
-   The USER token is deposited at a public-key-hash address (i.e. a wallet address).
-   The REFERENCE token is deposited at a particular script address, which is `addr_test1wrwjwf0hc2ge6qw8aq9n70mlmvm544v9f7fxnvwepjhemqsr0cy2k` .  We call this script *the Registry*.

    *Note:*  this address is obtained with

```shell
cardano-cli address build --payment-script-file registry.plutus --testnet-magic 2 --out-file registry.addr
```

-   The Registry has the property that only the Administrator (i.e. the wallet whose verification key is `wallet1.vkey`) can spend its contents.  (Thus, only the administrator can evolve the reference datums.)
-   The minting transaction will spend a particular UTxO, namely the one with reference `4a16c4f23a1855392496650deac62eb6c1fa1887f7a2e59ee2a3a3c7b67fa733#1` .  (This is stored in file `oref1a.tmp` .)
-   The REFERENCE token is deposited at the Registry together with a datum.
-   The structure of this datum is necessarily of the form specified above in Requirements.


## Suggested Exercise

Using **PyCardano**, construct transactions that try to violate one or more of the constraints specified above.

For reference, the shell script that builds a transaction (with **cardano-cli**) *satisfying  all the constraints* is provided in file `mint_txbuild.sh` .

(Also provided is the file `mint_execute_only_once.sh`, which builds, signs and submits the correct transaction.  Keep in mind that this script **can only be executed once**.)

### Files

All the files needed for off-chain code are in directory `ledger`.  The files with extension `.plutus` are serializations of the corresponding validator or minting scripts.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">File</th>
<th scope="col" class="org-left">Script</th>
<th scope="col" class="org-left">Parametrized by</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">registry.plutus</td>
<td class="org-left">Registry validator</td>
<td class="org-left">Administrator's pub-key-hash</td>
</tr>


<tr>
<td class="org-left">mint_1.plutus</td>
<td class="org-left">Minting policy</td>
<td class="org-left">TxOutRef (file oref1a.tmp)</td>
</tr>
</tbody>
</table>


## References

[1]  [CIP 68 - Datum Metadata Standard](https://cips.cardano.org/cips/cip68/) .

