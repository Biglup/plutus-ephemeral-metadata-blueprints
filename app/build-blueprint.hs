{-|
Module      : build-blueprint
Description : Application for building ephemeral metadata blueprints.
License     : Apache-2.0
Maintainer  : angel.castillob@protonmail.com
Stability   : experimental
-}

-- LANGUAGE EXTENSIONS --------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

-- IMPORTS --------------------------------------------------------------------

import Cardano.Api (ScriptDataJsonSchema (ScriptDataJsonDetailedSchema))
import Cardano.Api hiding (TxId)
import Cardano.Api.Shelley (fromPlutusData)
import Data.Function
import Data.Hex
import Ledger
import Ledger.Bytes (getLedgerBytes)
import qualified Cardano.Api.Shelley as Script
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Api as PlutusV1
import qualified Plutus.V2.Ledger.Api as PlutusV2

import Blueprints ( StringEncoding(..), Blueprint(..), Fungibility(..), Definitions(..), NumberType(..) )

-- DEFINITIONS ----------------------------------------------------------------

-- | Application entry point.
main :: IO ()
main = do
  let number = Number { nuType = Uint32, nuDefinitionName = "index", nuIndex = 2 }
  let blueprint = Blueprint { bFungibility = Fungible, bDataSources = [], bDefinitions = [number]}
  print blueprint

-- | Gets the JSON representation of the blueprint.
blueprintJSON :: Blueprint -> String
blueprintJSON blueprint = C.unpack $ A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ Script.fromPlutusData $ PlutusV2.toData blueprint)