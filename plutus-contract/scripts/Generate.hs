{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified NFTSale
import           Plutus.V1.Ledger.Api    (writeFileTextEnvelope)
import           PlutusTx.Prelude        hiding (Semigroup(..))
import           System.IO               (IO)

-- Example parameters: update with real testnet values as needed
exampleSaleParams :: NFTSale.SaleParams
exampleSaleParams = NFTSale.SaleParams
  { NFTSale.spSeller   = "ENTER_SELLER_PUBKEY_HASH_HERE"
  , NFTSale.spPrice    = 1000000  -- 1 ADA
  , NFTSale.spCurrency = "ENTER_POLICY_ID_HERE"
  , NFTSale.spToken    = "MyNFT"
  }

main :: IO ()
main = do
  let val = NFTSale.validator exampleSaleParams
  result <- writeFileTextEnvelope "nft-sale.plutus" Nothing val
  case result of
    Left err -> print err
    Right () -> putStrLn "nft-sale.plutus script written successfully!"
