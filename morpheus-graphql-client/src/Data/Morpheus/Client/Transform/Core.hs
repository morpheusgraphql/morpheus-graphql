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
    typeFrom,
    deprecationWarning,
  )
where

import Control.Monad.Except (MonadError)
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
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    VALID,
    VariableDefinitions,
    internal,
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

compileError :: GQLError -> GQLError
compileError x = internal $ "Unhandled Compile Time Error: \"" <> x <> "\" ;"

getType :: TypeName -> Converter (TypeDefinition ANY VALID)
getType typename =
  asks (typeDefinitions . fst)
    >>= selectBy (compileError $ " can't find Type" <> msg typename) typename

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
