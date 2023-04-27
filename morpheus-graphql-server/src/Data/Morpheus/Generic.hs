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
  )
where

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
  )
