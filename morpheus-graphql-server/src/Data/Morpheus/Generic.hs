module Data.Morpheus.Generic
  ( Gmap,
    gmap,
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
    CProxy (..),
    symbolName,
    CBox,
    -- x
    ScanRef (..),
    ProxyMap (..),
    useProxies,
    scan,
    scanNode,
    scanLeaf,
    runCBox,
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
import Data.Morpheus.Generic.GScan
import Data.Morpheus.Generic.Gmap
  ( Gmap,
    gmap,
  )
import Data.Morpheus.Generic.Proxy
  ( CBox,
    CProxy (..),
    runCBox,
    symbolName,
  )
import Data.Morpheus.Generic.RefType
  ( RefType (..),
  )
