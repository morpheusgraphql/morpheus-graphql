{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Transform.Core
  ( Converter (..),
    compileError,
    getType,
    leafType,
    typeFrom,
    deprecationWarning,
    customScalarTypes,
    UpdateT (..),
    resolveUpdates,
  )
where

import Control.Monad (foldM)
import Data.Morpheus.Client.Internal.Types
  ( ClientTypeDefinition (..),
  )
import Data.Morpheus.Error
  ( deprecatedField,
    globalErrorMessage,
  )
import Data.Morpheus.Ext.Result
  ( Eventless,
    Result (..),
  )
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    nameSpaceType,
    selectBy,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    Directives,
    FieldName,
    GQLErrors,
    Message,
    RAW,
    Ref (..),
    Schema (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    VALID,
    ValidationError,
    VariableDefinitions,
    hsTypeName,
    isNotSystemTypeName,
    lookupDeprecated,
    lookupDeprecatedReason,
    msg,
    toGQLError,
  )
import Relude

type Env = (Schema VALID, VariableDefinitions RAW)

newtype Converter a = Converter
  { runConverter ::
      ReaderT
        Env
        Eventless
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      Failure GQLErrors
    )

instance Failure ValidationError Converter where
  failure err = failure [toGQLError err]

newtype UpdateT m a = UpdateT {updateTState :: a -> m a}

resolveUpdates :: Monad m => a -> [UpdateT m a] -> m a
resolveUpdates a = foldM (&) a . fmap updateTState

compileError :: Message -> GQLErrors
compileError x =
  globalErrorMessage $ "Unhandled Compile Time Error: \"" <> x <> "\" ;"

getType :: TypeName -> Converter (TypeDefinition ANY VALID)
getType typename = asks fst >>= selectBy (compileError $ " can't find Type" <> msg typename) typename

customScalarTypes :: TypeName -> [TypeName]
customScalarTypes typeName
  | isNotSystemTypeName typeName = [typeName]
  | otherwise = []

leafType :: TypeDefinition a VALID -> Converter ([ClientTypeDefinition], [TypeName])
leafType TypeDefinition {typeName, typeContent} = fromKind typeContent
  where
    fromKind :: TypeContent TRUE a VALID -> Converter ([ClientTypeDefinition], [TypeName])
    fromKind DataEnum {} = pure ([], [typeName])
    fromKind DataScalar {} = pure ([], customScalarTypes typeName)
    fromKind _ = failure $ compileError "Invalid schema Expected scalar"

typeFrom :: [FieldName] -> TypeDefinition a VALID -> TypeName
typeFrom path TypeDefinition {typeName, typeContent} = __typeFrom typeContent
  where
    __typeFrom DataScalar {} = hsTypeName typeName
    __typeFrom DataObject {} = nameSpaceType path typeName
    __typeFrom DataInterface {} = nameSpaceType path typeName
    __typeFrom DataUnion {} = nameSpaceType path typeName
    __typeFrom _ = typeName

deprecationWarning :: Directives VALID -> (FieldName, Ref) -> Converter ()
deprecationWarning dirs (typename, ref) = case lookupDeprecated dirs of
  Just deprecation -> Converter $ lift $ Success {result = (), warnings, events = []}
    where
      warnings =
        deprecatedField
          typename
          ref
          (lookupDeprecatedReason deprecation)
  Nothing -> pure ()
