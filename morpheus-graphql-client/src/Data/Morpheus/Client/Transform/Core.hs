{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Client.Transform.Core
  ( Converter (..),
  )
where

--
-- MORPHEUS
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
  ( ReaderT (..),
  )
import Data.Morpheus.Error
  ( deprecatedField,
    globalErrorMessage,
  )
import Data.Morpheus.Internal.Utils
  ( nameSpaceType,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    ClientType (..),
    ConsD (..),
    DataEnumValue (..),
    DataTypeKind (..),
    FieldDefinition (..),
    GQLErrors,
    Key,
    Name,
    Operation (..),
    RAW,
    Ref (..),
    Schema (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TRUE,
    TypeContent (..),
    TypeD (..),
    TypeDefinition (..),
    TypeRef (..),
    UnionTag (..),
    VALID,
    Variable (..),
    VariableDefinitions,
    getOperationDataType,
    getOperationName,
    lookupDeprecated,
    lookupDeprecatedReason,
    removeDuplicates,
    toAny,
    typeFromScalar,
  )
import Data.Morpheus.Types.Internal.Operation
  ( Failure (..),
    Listable (..),
    keyOf,
    selectBy,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
    resolveUpdates,
  )
import Data.Semigroup ((<>))
import Data.Text

type Env = (Schema, VariableDefinitions RAW)

newtype Converter a = Converter
  { runConverter ::
      ReaderT
        Env
        Eventless
        a
  }
  deriving (Functor, Applicative, Monad, MonadReader Env)

instance Failure GQLErrors Converter where
  failure = Converter . lift . failure
