module Data.Morpheus.Client.CodeGen.Internal
  ( FromJSON (..),
    ToJSON (..),
    RequestType (..),
    Generic,
    OperationType (..),
    scalarFromJSON,
    scalarToJSON,
    invalidConstructorError,
    omitNulls,
    (.=),
    withObject,
    (.:),
    (.:?),
    takeValueType,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import Data.Morpheus.Client.Fetch.RequestType
  ( RequestType (..),
  )
import Data.Morpheus.Client.Internal.Utils (invalidConstructorError, omitNulls, takeValueType)
import Data.Morpheus.Types.GQLScalar (scalarFromJSON, scalarToJSON)
import Data.Morpheus.Types.Internal.AST (OperationType (..))
import GHC.Generics (Generic)
