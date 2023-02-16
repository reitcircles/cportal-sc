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
