module Data.Morpheus.Client.Internal.CodeGen
  ( FromJSON (..),
    ToJSON (..),
    RequestType (..),
    Generic,
    OperationType (..),
    scalarFromJSON,
    scalarToJSON,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
  )
import Data.Morpheus.Client.Fetch.RequestType
  ( RequestType (..),
  )
import Data.Morpheus.Types.GQLScalar (scalarFromJSON, scalarToJSON)
import Data.Morpheus.Types.Internal.AST (OperationType (..))
import GHC.Generics (Generic)
