const networkId = 0;

// general simple payment method for typhonjs
let generalizedPayment = async (inputs, outputs, collateral, requiredSigners, ttl, metadata=[]) => {
  /**
   * pParams is the global object from protocolParamters.js
   */
  const tx = new typhonjs.Transaction({ protocolParams: pParams })
  
  // add the inputs
  inputs.forEach(utxo => {
    tx.addInput(utxo);
  });

  // add the outputs
  outputs.forEach(txout => {
    tx.addOutput(txout)
  });

  // collateral
  if (jQuery.isEmptyObject(collateral) !== true) {
    tx.addCollateral(collateral)
  }

  // time to live
  tx.setTTL(ttl)

  if (jQuery.isEmptyObject(metadata) !== true) {
    tx.setAuxiliaryData(metadata)
  }
  
  // add signers
  if (requiredSigners.length !== 0) {
    requiredSigners.forEach(signer => {
      tx.addRequiredSigner(signer)
    });
  }

  return tx;
}; // end of payment

// first attempt at fee calc. there is a better way to do this.
let feeAdjust = async (start, end, fee, inputs, outputs, addr) => {
  const leftOver = start - end - fee;
  let FEE;
  const adaChange = txOutput(getAddress(addr), leftOver.toString(), [])
  const secondPass = outputs.concat([adaChange])
  const tx = await generalizedPayment(inputs, secondPass, {}, [], 999999999)
  FEE = (tx.calculateFee().c).reduce((a, b) => a + b, 0)
  // console.log(FEE)
  if (FEE - fee > 0) {
    return await feeAdjust(start, end, FEE, inputs, outputs, addr)
  }
  return FEE;
};

let scriptPointer = async (pathToPlutusFile, scriptAddress) => {
  // The smart contract must be stored locally to the js file.
  const plutusFile = await fetch(pathToPlutusFile);
  const plutus = await plutusFile.json();
  
  // create the script address
  const scriptObject = {
    hash: addrToPKH(scriptAddress),
    plutusScript: {
      cborHex: plutus.cborHex,
      type: plutus.type
    },
    type: 1
  };

  // construct address
  const script = new typhonjs.address.EnterpriseAddress(networkId, scriptObject);
  
  return script;
};


let getAddress = (address) => {
  return typhonjs.utils.getAddressFromBech32(address);
};


let txInput = (address, txId, index, amount, tokens, txDatum='', txRedeemer='') => {
  // address is address function types base, enterprise, etc
  let input = {};
  if (txDatum === '') {
    input = {
      address: address, 
      amount: new BigNumber(amount),
      index: index,
      tokens: tokens,
      txId: txId 
    };
  } else {
    input = {
      address: address, 
      amount: new BigNumber(amount),
      index: index,
      plutusData: txDatum,
      redeemer: txRedeemer,
      tokens: tokens,
      txId: txId 
    };
  }
  return input;
};


let txOutput = (address, amount, tokens, txDatum='') => {
  // address is base, enter, etc
  let output = {};
  if (txDatum === '') {
    output = { 
      address: address,
      amount: new BigNumber(amount),
      tokens: tokens
    };
  } else {
    output = { 
      address: address,
      amount: new BigNumber(amount),
      plutusData: txDatum,
      tokens: tokens
    };
  }
  return output;
};

// A token value.
let token = (amount, policyId, assetName) => {
  const asset = {
      policyId  : policyId,  // hex value
      assetName : assetName, // hex value
      amount    : new BigNumber(amount)    // string of integer
  };
  return asset;
};

let datum = (constructor, fields) => {
  const datumObject = {
    constructor: constructor,
    fields: fields,
  };
  return typhonjs.utils.createPlutusDataCbor(datumObject).toString("hex");
};

// address to payment public key hash
let addrToPKH = (addr) => {
  return typhonjs.utils.decodeBech32(addr).value.slice(2,58);
};

let addrToSKH = (addr) => {
  return typhonjs.utils.decodeBech32(addr).value.slice(58);
};

// pkh to address
let pkhToAddr = (pkh) => {
  if (networkId == 0) {
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


// input a map of token values and output an array of tokens
let decompressTokens = (tokens, sentDrip) => {
  let everything = []
  tokens.forEach((assetInfo, policyid) => {
    const pid = buffer.Buffer.from(policyid).toString('hex')
    assetInfo.forEach((amount, assetName) => {
        const tokenName = buffer.Buffer.from(assetName).toString('hex')
        everything.push(token(amount, pid, tokenName))
    });
  });
  return everything;
};


let findTxHashFromScript = (pkh, scriptAddress, apikey) => {
  let txdata = null;
  // get all utxos from script address
  $.ajax({
    async: false,
    url: 'https://cardano-testnet.blockfrost.io/api/v0/addresses/' + scriptAddress + '/utxos',
    beforeSend: function(xhr) {
      xhr.setRequestHeader("project_id", apikey)
    }, 
    success: function(data){
      // console.log('INSIDE CALL', data)
      // loop all the utxos
      data.forEach(item => {
        // get the datum value from specific utxo
        $.ajax({
          async: false,
          url: 'https://cardano-testnet.blockfrost.io/api/v0/scripts/datum/'+item.data_hash,
          beforeSend: function(xhr) {
            xhr.setRequestHeader("project_id", apikey)
          }, 
          success: function(datum) {
            if (datum.json_value.fields[1].bytes === pkh && item.amount.length === 2) {
              txdata = [item, datum]
            }
          } // end of datum success
        });// end of datum ajax
      }); // end of for
    } // end of utxos success
  }) // first utxos ajax
  return txdata;
};

let getLatestSlot = (apikey) => {
  
  let slotData = null;
  $.ajax({
    async: false,
    url: 'https://cardano-testnet.blockfrost.io/api/v0/blocks/latest',
    beforeSend: function(xhr) {
         xhr.setRequestHeader("project_id", apikey)
    }, success: function(data) {
      // console.log(data)
      slotData = data.slot
    }
  });
  return slotData;
};