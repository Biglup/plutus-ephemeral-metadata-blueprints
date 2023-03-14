{-|
Module      : Blueprints
Description : Blueprint data type definitions.
License     : Apache-2.0
Maintainer  : angel.castillob@protonmail.com
Stability   : experimental
-}

-- LANGUAGE EXTENSIONS --------------------------------------------------------

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

-- GHC OPTIONS ----------------------------------------------------------------

{-# OPTIONS_GHC -fno-ignore-interface-pragmas       #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas         #-}
{-# OPTIONS_GHC -fno-specialise                     #-}
{-# OPTIONS_GHC -fno-strictness                     #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- MODULE DEFINITION ----------------------------------------------------------

module Blueprints
(
  Blueprint(..),
  DataSource(..),
  Definitions(..),
  Fungibility(..),
  NumberType(..),
  StringEncoding(..)
) where

-- IMPORTS --------------------------------------------------------------------

import Data.Eq (Eq)
import GHC.Generics (Generic)
import PlutusTx.Builtins ( Integer, BuiltinByteString )
import qualified PlutusTx
import Text.Show ( Show )

-- DEFINITIONS ----------------------------------------------------------------

-- | Strings are fundamentally a sequence of characters, these characters can be represented as a secuence of bytes. The rules for translating
-- | between the character representation and the byte representation is called string encoding.
data StringEncoding =  Ascii -- ^ An ASCII character is 8 bits (1 byte).
                     | Utf8  -- ^ A Unicode character in UTF-8 encoding is between 8 bits (1 byte) and 32 bits (4 bytes).
                     | Utf16 -- ^ A Unicode character in UTF-16 encoding is between 16 bits (2 bytes) and 32 bits (4 bytes).
                     | Utf32 -- ^ A Unicode character in UTF-32 encoding is always 32 bits (4 bytes).
                     | Hex   -- ^ A hex encoded character is 8 bits (1 byte), every byte will be represented with two alphanumeric characters.
     deriving (Eq, Show, Generic)

-- | Specifies the sign and the size of a numeric value. Note: All number types are Little-Endian.
data NumberType =  Uint8   -- ^ 0 to 255 (1 byte)
                 | Uint16  -- ^ 0 to 65,535 (2 bytes)
                 | Uint32  -- ^ 0 to 4,294,967,295 (4 bytes)
                 | Uint64  -- ^ 0 to 18,446,744,073,709,551,615 (8 bytes)
                 | Int8    -- ^ -128 to 127 (1 byte)
                 | Int16   -- ^ -32,768 to 32,767 (2 bytes)
                 | Int32   -- ^ -2,147,483,648 to 2,147,483,647 (4 bytes)
                 | Int64   -- ^ -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807 (8 bytes)
                 | Float32 -- ^ -3.402823E+38 to 3.402823E+38 (4 bytes)
                 | Float64 -- ^ -1.7976931348623157E+308 to 1.7976931348623157E+308 (4 bytes)
    deriving (Eq, Generic, Show)

-- | Indicate whether tokens using this blueprint are fungible or non-fungible; please note that this is just a hint
-- | as it is impossible to know whether the token using the blueprint is truly non-fungible or not from just the metadata.
-- | If fungibility is critical to your off-chain application, you should take extra measures to make sure the tokens are
-- | withholding their fungibility intent.
data Fungibility = Fungible    -- ^ Fungible Tokens are a type of cryptographic tokens that are identical and similar in nature
                               --   and functionality. Two different fungible tokens serve the same purpose even when they are
                               --   divided or exchanged with other fungible tokens of the same type.
                 | NonFungible -- ^ A non-fungible token ( NFT) is a unique digital identifier that cannot be copied, substituted, or subdivided.
    deriving (Eq, Show, Generic)

-- | A DataSource is simply a set of data that can be indexed and consumed by blueprint definitions. These can be inline or remote.
data DataSource =   Inline -- ^ Inline data sources are embeeded in the blueprint in the form of an array. Definitions can then reference 
                    {      --   this information by specifying the name of the datasource and the index of the data they want to retrieve.
                      dsName:: !BuiltinByteString,     -- ^ The name of the data source, the name of data sources must be unique.
                      dsEntries:: ![BuiltinByteString] -- ^ The entries of the data source organized in a sequential array.
                    }
                  | Remote -- ^ Remote data sources are locate externally (I.E IPFS, Arweave etc) and must be fetched first before they can be referenced.
                    {      --   Fetching the remote data source must yield a JSON array.
                      dsName:: !BuiltinByteString, -- ^ The name of the data source, the name of data sources must be unique.
                      dsUrl:: !BuiltinByteString   -- ^ The data source remote URL.
                    } 
  deriving (Eq, Show, Generic)

-- | Specifies how to read a datasource index from the token name.
data DataSourceIndex =   Uint8DataSourceIndex -- ^ Read one byte at the position dIndex. (index can go from 0-255)
                         {
                           dsi8Index:: !Integer  -- ^ The position of the byte in the token name.
                         } 
                       | Uint16DataSourceIndex -- ^ Read two bytes at the position dIndex. (index can go from 0-65535)
                         {
                           dsi16dIndex:: !Integer   -- ^ The position of the byte in the token name.
                         }
  deriving (Eq, Show, Generic)

data Definitions =    Number -- ^ A numeral definition.
                      { 
                        nuDefinitionName:: !BuiltinByteString, -- ^ The name of the definition. Definition names must be unique.
                        nuType:: !NumberType,                  -- ^ The type of number.
                        nuIndex:: !Integer                     -- ^ The index in the Token name byte array.
                      }
                    | BitMask -- ^ Bitmkas definitions allow you to target specific bits in the Token name.
                      { 
                        bmDefinitionName:: !BuiltinByteString, -- ^ The name of the definition. Definition names must be unique.
                        bmIndex:: !Integer,                    -- ^ The index in the Token name byte array.
                        bmMask:: !BuiltinByteString            -- ^ The bit mask (I.E '00001010'). Bitmkas must be specified as a String of 8 characters only consisting of 1's and 0's.
                      }
                    | String -- ^ A string definition.
                      {
                        stDefinitionName:: !BuiltinByteString, -- ^ The name of the definition. Definition names must be unique.
                        stEncoding:: !StringEncoding,          -- ^ The encoding of the string.
                        stIndex:: !Integer,                    -- ^ The index in the Token name byte array.
                        stSize:: !Integer                      -- ^ The size of the string in bytes.
                      }
                    | FormattedString -- ^ A formatted string definition.
                      {
                        fmDefinitionName:: !BuiltinByteString, -- ^ The name of the definition. Definition names must be unique.
                        fmFormat:: !BuiltinByteString,         -- ^ The template string in qt-format.
                        fmParams:: ![BuiltinByteString]        -- ^ The list of definitions that will be use as arguments. Definitions must be specified by name.
                      }
                    | Image -- ^ A image definition.
                      {
                        imDefinitionName:: !BuiltinByteString, -- ^ The name of the definition. Definition names must be unique.
                        imMimeType:: !BuiltinByteString,       -- ^ The image MIME type.
                        imSource:: !BuiltinByteString          -- ^ The URL of the image.
                      }
                    | LayeredImage -- ^ A layered image definition. Layered images allow us to build completely new images from a set of layers.
                      {
                        liDefinitionName:: !BuiltinByteString, -- ^ The name of the definition. Definition names must be unique.
                        liMimeType:: !BuiltinByteString,       -- ^ The image MIME type.
                        liLayers:: ![BuiltinByteString]        -- ^ The list of URLs that form the image (ordered, I.E dLayers[0] is layer 0).
                      }
                    | File -- ^ A file definition.
                      { 
                        fDefinitionName:: !BuiltinByteString, -- ^ The name of the definition. Definition names must be unique.
                        fMediaType:: !BuiltinByteString,      -- ^ The file media type.
                        fSource:: !BuiltinByteString          -- ^ The URL of the file.
                      }
                    | Files -- ^ A file list definition.
                      { 
                        fsDefinitionName:: !BuiltinByteString, -- ^ The name of the definition. Definition names must be unique.
                        fsMediaType:: !BuiltinByteString,      -- ^ The file media type.
                        fsFiles:: ![BuiltinByteString]         -- ^ The list of files URLs.
                      }
                    | DataSourceString -- A data sourced string.
                      { 
                        dsDefinitionName:: !BuiltinByteString, -- ^ The name of the definition. Definition names must be unique.
                        dsDataSource:: !BuiltinByteString,     -- ^ The name of the data source.
                        dsIndex:: !DataSourceIndex             -- ^ The index in the data source.
                      }
                    | DataSourceImage -- A data sourced image.
                      { 
                        diDefinitionName:: !BuiltinByteString, -- ^ The name of the definition. Definition names must be unique.
                        diMimeType:: !BuiltinByteString,       -- ^ The image MIME type.
                        diDataSource:: !BuiltinByteString,     -- ^ The name of the data source.
                        diIndex:: !DataSourceIndex             -- ^ The index in the data source.
                      }
                    | DataSourceLayeredImage -- A data sourced layered image.
                      { 
                        dlDefinitionName:: !BuiltinByteString, -- ^ The name of the definition. Definition names must be unique.
                        dlMimeType:: !BuiltinByteString,       -- ^ The image MIME type.
                        dlLayers:: ![DataSourceIndex]          -- ^ The index in the data source
                      }
                    | DataSourceFile
                      { 
                        dfDefinitionName:: !BuiltinByteString,
                        dfMediaType:: !BuiltinByteString,
                        dfDataSource:: !BuiltinByteString,
                        dfIndex:: !DataSourceIndex 
                      }
                    | DataSourceFiles
                      { 
                        dfsDefinitionName:: !BuiltinByteString,
                        dfsMediaType:: !BuiltinByteString,
                        dfsFiles :: ![BuiltinByteString]
                      }
                    | SubDefinitions
                      { 
                        suDefinitionName:: !BuiltinByteString,
                        suSubDefinitions :: ![Definitions]
                      } 
  deriving (Eq, Show, Generic)

data Blueprint = Blueprint {
  bFungibility:: !Fungibility,
  bDataSources:: ![DataSource],
  bDefinitions:: ![Definitions]
} deriving (Eq, Show, Generic)

-- LIFT TO PLUTUS  ------------------------------------------------------------

PlutusTx.makeIsDataIndexed ''StringEncoding [
    ('Ascii, 0),
    ('Utf8,  1),
    ('Utf16, 2),
    ('Utf32, 3),
    ('Hex,   4)
  ]

PlutusTx.makeIsDataIndexed ''NumberType [
    ('Uint8,   0),
    ('Uint16,  1),
    ('Uint32,  2),
    ('Uint64,  3),
    ('Int8,    4),
    ('Int16,   5),
    ('Int32,   6),
    ('Int64,   7),
    ('Float32, 8),
    ('Float64, 9)
  ]

PlutusTx.makeIsDataIndexed ''Fungibility [
    ('Fungible,    0),
    ('NonFungible, 1)
  ]

PlutusTx.makeIsDataIndexed ''DataSource [
    ('Inline, 0),
    ('Remote, 1)
  ]

PlutusTx.makeIsDataIndexed ''Definitions [
    ('Number, 0),
    ('BitMask, 1),
    ('String, 2),
    ('FormattedString, 3),
    ('Image, 4),
    ('LayeredImage, 5),
    ('File, 6),
    ('Files, 7),
    ('DataSourceString, 8),
    ('DataSourceImage, 9),
    ('DataSourceLayeredImage, 10),
    ('DataSourceFile, 11),
    ('DataSourceFiles, 12),
    ('SubDefinitions, 13)
  ]

PlutusTx.unstableMakeIsData ''Blueprint

PlutusTx.makeIsDataIndexed ''DataSourceIndex [
    ('Uint8DataSourceIndex, 0),
    ('Uint16DataSourceIndex, 1)
  ]

PlutusTx.makeLift ''DataSourceIndex
PlutusTx.makeLift ''StringEncoding
PlutusTx.makeLift ''NumberType
PlutusTx.makeLift ''Blueprint
PlutusTx.makeLift ''Fungibility
PlutusTx.makeLift ''DataSource
PlutusTx.makeLift ''Definitions