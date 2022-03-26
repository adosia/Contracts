$(document).ready(async function () {
    // Get References
    const $connectButton = $('button#connectButton');
    const $retrieveVestmentButton = $('button#retrieveVestmentButton');
    const $sendADAButton = $('button#sendADAButton');
    let TyphonWallet = null;

    // Helper functions
    const isEnabled = async () => {
        try {
            const result = await TyphonWallet.isEnabled();
            return result.status === true && result.data === true;
        } catch (e) {
            return false;
        }
    };
    const enableWallet = async () => {
        return await TyphonWallet.enable();
    };

    // Init
    setTimeout(async function () {
        TyphonWallet = window.cardano.typhon;
        if (await isEnabled()) {
            $connectButton.hide();
            $retrieveVestmentButton.show();
            $sendADAButton.show();
        } else {
            $connectButton.removeAttr('disabled');
        }
    }, 500);

    // Handle connect
    $connectButton.click(async function () {
        const result = await enableWallet();
        if (result.status === true) {
            $connectButton.hide();
            $retrieveVestmentButton.show();
            $sendADAButton.show();

        } else {
            alert('You declined to connect the wallet!');
        }
    });
    
    //HERE
    $sendADAButton.click(async function () {
        console.log('ADA')
        console.log("current datum")
        const currentTxDatum = datum(
            0,
            byteBuffer("34881042baedd061814b9cdef28aa4cc2fc3e712e0c2f3746003ea96"),
            [byteBuffer("a2108b7b1704f9fe12c906096ea1634df8e089c9ccfd651abae4a439")],
            [100],
            byteBuffer("db7bffc41a43c4d9c31342e3fd457409aeb40302aa52058df374913b"),
            [5, 0],
            [0, 1000]
        );
        console.log(currentTxDatum)
        // const profitOutput = txOut(profitAddress, "10000000", [], currentTxDatum);
        console.log('call endpoint')
        const tx = await sendADA(profitAddress, [], currentTxDatum, false)
        console.log(tx)
        console.log('done')
    });

    // create a sale
    $retrieveVestmentButton.click(async function () {
        
        console.log('Retrieve Vestment Sale')
        const baseAddress = (await TyphonWallet.getAddress()).data
        const paymentPKH = addrToPKH(baseAddress)
        const paymentAddr = pkhToAddr(paymentPKH)
        console.log(baseAddress)
        console.log(paymentPKH)
        console.log(paymentAddr)

        // just pick the first one to test it
        const tkn = {policyId: '57fca08abbaddee36da742a839f7d83a7e1d2419f1507fcbf3916522', assetName: '43484f43'}
        const vestingReturn = token("1000", tkn.policyId, tkn.assetName)
        const vestingPayout = token("1000", tkn.policyId, tkn.assetName)
        console.log(vestingReturn);
        console.log(vestingPayout);

        console.log("redeemer")
        const txAction = redeemer(0)
        console.log(txAction)
        
        console.log("current datum")
        const currentTxDatum = datum(
            0,
            byteBuffer("34881042baedd061814b9cdef28aa4cc2fc3e712e0c2f3746003ea96"),
            [byteBuffer("a2108b7b1704f9fe12c906096ea1634df8e089c9ccfd651abae4a439")],
            [100],
            byteBuffer("db7bffc41a43c4d9c31342e3fd457409aeb40302aa52058df374913b"),
            [5, 0],
            [0, 1000]
        )
        console.log(currentTxDatum);
        console.log("next datum")
        const nextTxDatum = datum(
            0,
            byteBuffer("34881042baedd061814b9cdef28aa4cc2fc3e712e0c2f3746003ea96"),
            [byteBuffer("a2108b7b1704f9fe12c906096ea1634df8e089c9ccfd651abae4a439")],
            [100],
            byteBuffer("db7bffc41a43c4d9c31342e3fd457409aeb40302aa52058df374913b"),
            [5, 0],
            [0, 1000]
        )
        console.log(nextTxDatum);


        const profitOutput = txOut(profitAddress, "1000000", []);
        const payoutOutput = txOut(paymentAddr, "1689618", [vestingPayout]);
        const returnOutput = txOut(scriptAddress, "1689618", [vestingReturn], nextTxDatum);
        
        // 
        fetch('./vesting_contract.plutus')
        .then(response => response.json())
        .then(async (data) => {
            console.log("Building TX");
            const txId = "0e0d3236cdc54a40012f705779f151934a8f3f06aeb9a7a881094b1b4b894b66";
            const index = 2;
            const plutusScript = data.cborHex;
            // build script utxo
            const scriptUTxO = scriptInput(txId, index, currentTxDatum, txAction, {mem: 3138206, steps: 1081621378}, plutusScript);
            console.log(scriptUTxO)
            console.log('Retrieve Vestment')
            const tx = await retrieveVestment(scriptUTxO, [profitOutput, payoutOutput, returnOutput], paymentPKH, false)
            console.log(tx)
            console.log('done')
        })
        .catch(error => console.log(error));
    }); // end of endpoint
});