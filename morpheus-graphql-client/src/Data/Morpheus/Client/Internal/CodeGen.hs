module Data.Morpheus.Client.Internal.CodeGen
  ( FromJSON (..),
    ToJSON (..),
    RequestType (..),
    Generic,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
  )
import Data.Morpheus.Client.Fetch.RequestType
  ( RequestType (..),
  )
import GHC.Generics (Generic)
