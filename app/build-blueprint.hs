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

import Blueprints ( DataSourceIndex(..), StringEncoding(..), Blueprint(..), Fungibility(..), Definitions(..), NumberType(..), DataSource(..) )

-- DEFINITIONS ----------------------------------------------------------------

-- | Application entry point.
main :: IO ()
main = do
  let blueprint = Blueprint { 
    bFungibility = NonFungible, 
    bDataSources = [
        Remote { dsName = "goals",  dsUrl = "ipfs://bafkreifjz25l7y4sjtpdxttmw2xj2j6xgvsr3zqsqcit4gtt6ch3d2pr5u"},
        Remote { dsName = "names",  dsUrl = "ipfs://bafkreihhirxeyf5uso2n2ndrypb4mmkfamzfacfmnk66ky7n4jb72plabm"},
        Remote { dsName = "quotes", dsUrl = "ipfs://bafkreigyzpopz577uvrqbxwqsjbnds5dibk62dltmn7qp5i6bfwrshkooq"},
        Remote { dsName = "l0", dsUrl = "ipfs://bafkreiajgr7fa3mcx4oqlfymjtwow5cn5fqhisjyv2r33xygzozks56d6m"},
        Remote { dsName = "l1", dsUrl = "ipfs://bafkreiaa4owiroyybewwp5a2sf5kg2kbqg2wfb55tk7gezv3pdbxsokhiq"},
        Remote { dsName = "l2", dsUrl = "ipfs://bafkreigee5bdvg37dnft5snabtkoos5s6kpgzy47qeaaccc2twrr4prwr4"},
        Remote { dsName = "l3", dsUrl = "ipfs://bafkreiepl5e2q4dayxmnol4pbl7dy3gdasj7uqz3c26oye7txi6dkyrhjq"},
        Remote { dsName = "l4", dsUrl = "ipfs://bafkreiekzegzbq3kegv4oc43sve7oo42y4awp36g2zbd3e7bxtvoe5wgda"},
        Remote { dsName = "l5", dsUrl = "ipfs://bafkreifeyotbcpv5zur45ocivrexu2dv7fvh4pnx7o7j4emgylfsfxhqkm"},
        Remote { dsName = "l6", dsUrl = "ipfs://bafkreicn3e6rwoejldk5pzik3qacadhixppuh6wq2z5agdj32u6vtqcgte"},
        Remote { dsName = "l7", dsUrl = "ipfs://bafkreicope64wg4v64ygfenjtrx4mlacck7iqmuqj3zgc34rg6fsjpasoq"},
        Remote { dsName = "l8", dsUrl = "ipfs://bafkreihbiqfdlhc2t6kpuapwjzhwrgojesxj3ode55f2ihmldf6vqp2myi"},
        Remote { dsName = "l9", dsUrl = "ipfs://bafkreiczyoihm2x5andvokyxngu3iamwib5twjxgtujfx2rtb3kydsqkum"}
    ], bDefinitions = [
      ConstantString { cstDefinitionName = "website", cstValue = "https://ephemeral-squares.io" },
      ConstantNumber { cnuDefinitionName = "version", cnuValue = 1 },
      DataSourceString { dsDefinitionName = "goal", dsIndex = Uint8DataSourceIndex { dsi8Name = "goals", dsi8Index = 2 } },
      DataSourceString { dsDefinitionName = "name", dsIndex = Uint8DataSourceIndex { dsi8Name = "names", dsi8Index = 3 } },
      DataSourceString { dsDefinitionName = "quote", dsIndex = Uint8DataSourceIndex { dsi8Name = "quotes", dsi8Index = 4 } },
      DataSourceLayeredImage { 
        dlDefinitionName = "image",
        dlMimeType = "img/svg",
        dlLayers = [
            Uint8DataSourceIndex { dsi8Name = "l0", dsi8Index = 18 },
            Uint8DataSourceIndex { dsi8Name = "l1", dsi8Index = 19 },
            Uint8DataSourceIndex { dsi8Name = "l2", dsi8Index = 20 },
            Uint8DataSourceIndex { dsi8Name = "l3", dsi8Index = 21 },
            Uint8DataSourceIndex { dsi8Name = "l4", dsi8Index = 22 },
            Uint8DataSourceIndex { dsi8Name = "l5", dsi8Index = 23 },
            Uint8DataSourceIndex { dsi8Name = "l6", dsi8Index = 24 },
            Uint8DataSourceIndex { dsi8Name = "l7", dsi8Index = 25 },
            Uint8DataSourceIndex { dsi8Name = "l9", dsi8Index = 26 },
            Uint8DataSourceIndex { dsi8Name = "l0", dsi8Index = 27 }
          ]
      }
    ]}
  print $ blueprintJSON blueprint

-- | Gets the JSON representation of the blueprint.
blueprintJSON :: Blueprint -> String
blueprintJSON blueprint = C.unpack $ A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ Script.fromPlutusData $ PlutusV2.toData blueprint)