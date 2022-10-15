{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Transform.Core
  ( Converter (..),
    compileError,
    getType,
    typeFrom,
    deprecationWarning,
    toCodeGenField,
    toClientDeclarations,
  )
where

import Control.Monad.Except (MonadError)
import Data.Morpheus.Client.Internal.AST
  ( ClientDeclaration (..),
    ClientTypeDefinition (..),
    DERIVING_MODE (..),
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenField (..),
    CodeGenType (..),
    DerivingClass (..),
    FIELD_TYPE_WRAPPER (..),
  )
import Data.Morpheus.CodeGen.Utils (camelCaseTypeName)
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
    FieldDefinition (..),
    FieldName,
    GQLError,
    RAW,
    Ref (..),
    Schema (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    VALID,
    VariableDefinitions,
    internal,
    isNullable,
    isResolverType,
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

toCodeGenField :: FieldDefinition a b -> CodeGenField
toCodeGenField FieldDefinition {fieldType = field@TypeRef {..}, ..} =
  CodeGenField
    { fieldName,
      fieldType = typeConName,
      wrappers = [GQL_WRAPPER typeWrappers],
      fieldIsNullable = isNullable field
    }

toClientDeclarations :: ClientTypeDefinition -> [ClientDeclaration]
toClientDeclarations def@ClientTypeDefinition {clientKind}
  | KindScalar == clientKind = [FromJSONClass SCALAR_MODE cgType, ToJSONClass SCALAR_MODE cgType]
  | KindEnum == clientKind = [ClientType cgType, FromJSONClass ENUM_MODE cgType, ToJSONClass ENUM_MODE cgType]
  | isResolverType clientKind = [ClientType cgType, FromJSONClass TYPE_MODE cgType]
  | otherwise = [ClientType cgType, ToJSONClass TYPE_MODE cgType]
  where
    cgType = printClientType def

printClientType :: ClientTypeDefinition -> CodeGenType
printClientType ClientTypeDefinition {..} =
  CodeGenType
    { cgTypeName = clientTypeName,
      cgConstructors = clientCons,
      cgDerivations = [GENERIC, SHOW, CLASS_EQ]
    }
