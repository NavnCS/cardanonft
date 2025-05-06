{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}

module NFTSale where

import           Plutus.V1.Ledger.Api
import           Plutus.V1.Ledger.Contexts
import qualified PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (Show)
import           Plutus.V1.Ledger.Value     (flattenValue)

--------------------------------------------------
-- | Parameters for the NFT sale
--------------------------------------------------
data SaleParams = SaleParams
    { spSeller   :: PubKeyHash
    , spPrice    :: Integer
    , spCurrency :: CurrencySymbol
    , spToken    :: TokenName
    }
    deriving Show

PlutusTx.makeLift ''SaleParams

--------------------------------------------------
-- | Validator logic
--------------------------------------------------
{-# INLINABLE mkNFTSaleValidator #-}
mkNFTSaleValidator :: SaleParams -> () -> () -> ScriptContext -> Bool
mkNFTSaleValidator sp _ _ ctx =
    traceIfFalse "Wrong amount paid" correctPricePaid &&
    traceIfFalse "NFT not transferred to buyer" nftTransferred

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Check that the buyer has paid the correct amount to the seller
    correctPricePaid :: Bool
    correctPricePaid =
        let valuePaid = valuePaidTo info (spSeller sp)
        in valueOf valuePaid adaSymbol adaToken >= spPrice sp

    -- Check that the NFT has been transferred to the buyer
    nftTransferred :: Bool
    nftTransferred =
        case getContinuingOutputs ctx of
            [o] -> assetClassValueOf (txOutValue o) (assetClass (spCurrency sp) (spToken sp)) == 0
            _   -> False

--------------------------------------------------
-- | Boilerplate
--------------------------------------------------
typedNFTSaleValidator :: SaleParams -> TypedValidator ()
typedNFTSaleValidator sp = mkTypedValidator @()
    ($$(PlutusTx.compile [|| \p -> mkNFTSaleValidator p ||]) `PlutusTx.applyCode` PlutusTx.liftCode sp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()

validator :: SaleParams -> Validator
validator = validatorScript . typedNFTSaleValidator

scriptAddress :: SaleParams -> Address
scriptAddress = scriptAddress . validator
