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
    -- Others
    CountFields (..),
  )
where

import Data.Morpheus.Generic.CountFields
  ( CountFields (..),
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
  )
