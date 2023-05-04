module Data.Morpheus.Generic
  ( Gmap,
    GmapFun (..),
    -- GRep
    GRep,
    GRepCons (..),
    GRepFun (..),
    GRepField (..),
    GRepType (..),
    GRepValue (..),
    deriveType,
    deriveValue,
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
    GRepField (..),
    GRepFun (..),
    GRepType (..),
    GRepValue (..),
    deriveType,
    deriveValue,
  )
import Data.Morpheus.Generic.Gmap
  ( Gmap,
    GmapFun (..),
    runGmap,
  )
import Data.Morpheus.Generic.Proxy
  ( CProxy (..),
    symbolName,
  )
import Data.Morpheus.Generic.RefType
  ( RefType (..),
  )
