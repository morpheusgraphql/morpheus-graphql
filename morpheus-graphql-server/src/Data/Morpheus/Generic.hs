module Data.Morpheus.Generic
  ( Gmap,
    GmapFun (..),
    -- GRep
    GRep,
    GRepCons (..),
    GRepContext (..),
    GRepField (..),
    GRepType (..),
    GRepValue (..),
    deriveType,
    deriveValue,
    scanTypes,
    -- fields
    DecoderFun (..),
    DecodeFields,
    decodeFields,
    CountFields (..),
    RefType (..),
    DescribeCons (..),
    runGmap,
    CProxy (..),
    symbolName,
  )
where

import Data.Morpheus.Generic.Cons
  ( DescribeCons (..),
  )
import Data.Morpheus.Generic.Fields
  ( CountFields (..),
    DecodeFields,
    DecoderFun (..),
    decodeFields,
  )
import Data.Morpheus.Generic.GRep
  ( GRep,
    GRepCons (..),
    GRepContext (..),
    GRepField (..),
    GRepType (..),
    GRepValue (..),
    deriveType,
    deriveValue,
    scanTypes,
  )
import Data.Morpheus.Generic.Gmap
  ( Gmap,
    GmapFun (..),
    runGmap,
  )
import Data.Morpheus.Generic.Proxy
  ( CProxy (..),
    isRecordProxy,
    symbolName,
  )
import Data.Morpheus.Generic.RefType
  ( RefType (..),
  )
