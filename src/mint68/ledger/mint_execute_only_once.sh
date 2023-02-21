cardano-cli transaction build \
	    --babbage-era \
	    --testnet-magic 2 \
	    --tx-in $(cat oref1a.tmp) \
	    --required-signer wallet1.skey \
	    --tx-in-collateral $(cat oref1b.tmp) \
	    --tx-out $(cat registry.addr)+"2000000 lovelace"+"1 $(cat mint_1.policy).$(cat refTokenName.hex)" \
            --tx-out-inline-datum-file datum1.json \
            --tx-out $(cat wallet2.addr)+"2000000 lovelace"+"1 $(cat mint_1.policy).$(cat usrTokenName.hex)" \
	    --change-address $(cat wallet1.addr) \
	    --mint "1 $(cat mint_1.policy).$(cat refTokenName.hex)"+"1 $(cat mint_1.policy).$(cat usrTokenName.hex)" \
	    --mint-script-file mint_1.plutus \
	    --mint-redeemer-file unit.json \
	    --protocol-params-file protocol-params.json \
	    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file wallet1.skey \
    --testnet-magic 2 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file tx.signed
