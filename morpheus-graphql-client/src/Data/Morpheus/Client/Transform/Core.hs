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

import Control.Monad.Except (MonadError (throwError))
import Data.Morpheus.Client.Internal.Types
  ( ClientTypeDefinition (..),
  )
import Data.Morpheus.CodeGen.Internal.TH
  ( camelCaseTypeName,
  )
import Data.Morpheus.Error
  ( deprecatedField,
  )
import Data.Morpheus.Internal.Ext
  ( GQLResult,
    Result (..),
  )
import Data.Morpheus.Internal.Utils
  ( selectBy,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    Directives,
    FieldName,
    GQLError,
    RAW,
    Ref (..),
    Schema (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    VALID,
    VariableDefinitions,
    internal,
    isNotSystemTypeName,
    lookupDeprecated,
    lookupDeprecatedReason,
    msg,
    typeDefinitions,
  )
import Relude

type Env = (Schema VALID, VariableDefinitions RAW)

newtype Converter a = Converter
  { runConverter ::
      ReaderT
        Env
        GQLResult
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadError GQLError
    )

newtype UpdateT m a = UpdateT {updateTState :: a -> m a}

resolveUpdates :: Monad m => a -> [UpdateT m a] -> m a
resolveUpdates a = foldlM (&) a . fmap updateTState

compileError :: GQLError -> GQLError
compileError x = internal $ "Unhandled Compile Time Error: \"" <> x <> "\" ;"

getType :: TypeName -> Converter (TypeDefinition ANY VALID)
getType typename =
  asks (typeDefinitions . fst)
    >>= selectBy (compileError $ " can't find Type" <> msg typename) typename

customScalarTypes :: TypeName -> [TypeName]
customScalarTypes typeName
  | isNotSystemTypeName typeName = [typeName]
  | otherwise = []

leafType :: TypeDefinition a VALID -> Converter [TypeName]
leafType TypeDefinition {typeName, typeContent} = fromKind typeContent
  where
    fromKind :: TypeContent TRUE a VALID -> Converter [TypeName]
    fromKind DataEnum {} = pure [typeName]
    fromKind DataScalar {} = pure (customScalarTypes typeName)
    fromKind _ = throwError $ compileError "Invalid schema Expected scalar"

typeFrom :: [FieldName] -> TypeDefinition a VALID -> TypeName
typeFrom path TypeDefinition {typeName, typeContent} = __typeFrom typeContent
  where
    __typeFrom DataObject {} = camelCaseTypeName path typeName
    __typeFrom DataInterface {} = camelCaseTypeName path typeName
    __typeFrom DataUnion {} = camelCaseTypeName path typeName
    __typeFrom _ = typeName

deprecationWarning :: Directives VALID -> (FieldName, Ref FieldName) -> Converter ()
deprecationWarning dirs (typename, ref) = case lookupDeprecated dirs of
  Just deprecation -> Converter $ lift $ Success {result = (), warnings}
    where
      warnings =
        [ deprecatedField
            typename
            ref
            (lookupDeprecatedReason deprecation)
        ]
  Nothing -> pure ()
