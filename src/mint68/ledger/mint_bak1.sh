cardano-cli transaction build \
	    --testnet-magic 1 \
	    --tx-in $(cat oref1.tmp) \
	    --tx-out $(cat registry.addr)+"2000000 lovelace"+"1 2a9a1af10f76317c80b8c2833292a8f467ef764985bea0388b57f580.(100)REFERENCE" \
	    --tx-out $(cat wallet2.addr)+"2000000 lovelace"+"1 2a9a1af10f76317c80b8c2833292a8f467ef764985bea0388b57f580.(222)USER" \
	    --tx-out-datum-inline-datum-file datum0.json \
	    --change-address $(cat wallet1.addr) \
	    --mint "1 2a9a1af10f76317c80b8c2833292a8f467ef764985bea0388b57f580.(100)REFERENCE"+"1 2a9a1af10f76317c80b8c2833292a8f467ef764985bea0388b57f580.(222)USER"
	    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file wallet1.skey \
    --testnet-magic 1 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1 \
    --tx-file tx.signed
