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
