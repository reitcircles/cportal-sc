cardano-cli transaction sign \
    --tx-body-file tx_mint.body \
    --signing-key-file wallet1.skey \
    --testnet-magic 2 \
    --out-file tx_mint.signed

cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file tx_mint.signed
