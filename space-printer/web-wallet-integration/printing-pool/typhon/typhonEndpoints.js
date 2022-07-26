let scriptAddress = "addr_test1wq0qml9nxrup2yygfn74844x06wmt474e06celvun3f2xxgdrknaj";
let profitAddress = "addr_test1vqrvxke4v7ed3axr4quvgszslfu9cupd2vjx0j9lmwzsg6czta55j";
let profitPKH = "06c35b3567b2d8f4c3a838c44050fa785c702d532467c8bfdb85046b";

// address to payment public key hash; network agnostic
let addrToPKH = (addr) => {
    return typhonjs.utils.decodeBech32(addr).value.slice(2,58);
};

// pkh to address; needs network; default to testnet
let pkhToAddr = (pkh, network=0) => {
    if (network == 0) {
        return typhonjs.utils.getAddressFromHex("60"+pkh).addressBech32
    } else {
        return typhonjs.utils.getAddressFromHex("61"+pkh).addressBech32
    }
};

// Puts strings into the correct buffer form.
let byteBuffer = (stringInput) => {
    return buffer.Buffer.from(stringInput, "hex");
};

// Puts a string into the correct hex form.
let stringToHex = (string) => {
    return new buffer.Buffer(string).toString('hex');
};

// A token value.
let token = (amount, policyId, assetName) => {
    const asset = {
        policyId  : policyId,  // hex value
        assetName : assetName, // hex value
        amount    : amount     // string of integer
    };
    return asset;
};

// The vesting datum object.
let datum = (vestingStage, vestingUserPKH, vestingGroupPKH, vestingWeights, treasuryPKH, deadlineParams, rewardParams) => {
    const datumObject = {
        constructor: 0,
        fields: [
            vestingStage, 
            vestingUserPKH, 
            vestingGroupPKH,
            vestingWeights,
            treasuryPKH,
            deadlineParams,
            rewardParams,
        ],
    };
    return typhonjs.utils.createPlutusDataCbor(datumObject).toString("hex");
};

// The vesting redeemer object.
let redeemer = (action) => {
    const redeemerObject = {
        constructor: 0,
        fields: [
            action
        ],
    };
    return typhonjs.utils.createPlutusDataCbor(redeemerObject).toString("hex");
};

// script utxo
let scriptInput = (txId, index, currentDatum, requiredRedeemer, units, plutusScript) => {
    const utxo = {
        txId: txId,
        index: index,
        plutusDataCbor: currentDatum,
        redeemer: {
            plutusDataCbor: requiredRedeemer, // cbor hex string
            exUnits: {
                mem: units.mem,
                steps: units.steps
            } // estimated ex units
        },
        paymentScript: {
            cborHex: plutusScript,
            type: "PlutusScriptV1"
        },
    };
    return utxo
};


let txOut = (address, amount, tokens, nextDatum='') => {
    let output;
    if (nextDatum === '') {
        output = {
            address: address,
            amount: amount, // Lovelace string
            tokens: tokens
        };
    } else {
        output = {
            address: address,
            amount: amount, // Lovelace string
            tokens: tokens,
            plutusDataCbor: nextDatum
        };
    }
    return output;
};


// Build the Tx
let plutusTx = async (inputs, outputs, requiredSigners=[], submit=false) => {
    let plutusTransactionResponse;
    if (requiredSigners.length === 0) {
        plutusTransactionResponse = await window.cardano.typhon.plutusTransaction({
            inputs: inputs,
            outputs: outputs,
            submit: submit
        });    
    } else {
        plutusTransactionResponse = await window.cardano.typhon.plutusTransaction({
            inputs: inputs,
            outputs: outputs,
            requiredSigners: requiredSigners,
            submit: submit
        });
    }
    return plutusTransactionResponse;
};


// Check the txStatus
let txStatus = async (transactionId) => {
    return await window.cardano.typhon.getTransactionStatus([transactionId]);
}


// Retrieve Sale Endpoint
let retrieveVestment = (scriptUTxO, retrieveOutput, walletPKH, submit=false) => {
    return plutusTx([scriptUTxO], retrieveOutput, [walletPKH], submit)
};

// Create Sale Endpoint
let sendADA = async (scriptAddr, tokens, datumCbor, metadata, submit=false) => {
    const paymentTransactionResponse = await window.cardano.typhon.paymentTransaction({
        outputs: [
            {
                address: scriptAddr,
                tokens: tokens,
                plutusDataCbor: datumCbor,
            },
        ],
        auxiliaryDataCbor: metadata,
        submit: submit
    });
    return paymentTransactionResponse;
};