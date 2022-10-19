{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.CodeGen.Interpreting.Core
  ( LocalM (..),
    compileError,
    getType,
    typeFrom,
    deprecationWarning,
    printClientType,
    defaultDerivations,
    gqlWarning,
    LocalContext (..),
    runLocalM,
    withPosition,
    getNameByPath,
  )
where

import Control.Monad.Except (MonadError)
import Data.Morpheus.Client.CodeGen.AST
  ( ClientTypeDefinition (..),
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenType (..),
    CodeGenTypeName (..),
    DerivingClass (..),
    fromTypeName,
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
    Position,
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

data LocalContext = LocalContext
  { ctxSchema :: Schema VALID,
    ctxVariables :: VariableDefinitions RAW,
    ctxPosition :: Maybe Position
  }

runLocalM :: LocalContext -> LocalM a -> GQLResult a
runLocalM context = flip runReaderT context . _runLocalM

withPosition :: Position -> LocalM a -> LocalM a
withPosition pos = local (\ctx -> ctx {ctxPosition = Just pos})

newtype LocalM a = LocalM
  { _runLocalM ::
      ReaderT
        LocalContext
        GQLResult
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader LocalContext,
      MonadError GQLError
    )

compileError :: GQLError -> GQLError
compileError x = internal $ "Unhandled Compile Time Error: \"" <> x <> "\" ;"

getType :: TypeName -> LocalM (TypeDefinition ANY VALID)
getType typename =
  asks (typeDefinitions . ctxSchema)
    >>= selectBy (compileError $ " can't find Type" <> msg typename) typename

typeFrom :: [FieldName] -> TypeDefinition a VALID -> CodeGenTypeName
typeFrom path TypeDefinition {typeName, typeContent} = __typeFrom typeContent
  where
    __typeFrom DataObject {} = getNameByPath path typeName
    __typeFrom DataInterface {} = getNameByPath path typeName
    __typeFrom DataUnion {} = getNameByPath path typeName
    __typeFrom _ = fromTypeName typeName

getNameByPath :: [FieldName] -> TypeName -> CodeGenTypeName
getNameByPath path tName = case reverse path of
  (p : ps) -> CodeGenTypeName {namespace = reverse ps, typeParameters = [], typename = coerce p}
  [] -> CodeGenTypeName {namespace = [], typeParameters = [], typename = tName}

deprecationWarning :: Directives VALID -> (FieldName, Ref FieldName) -> LocalM ()
deprecationWarning dirs (typename, ref) = case lookupDeprecated dirs of
  Just deprecation -> gqlWarning $ deprecatedField typename ref (lookupDeprecatedReason deprecation)
  Nothing -> pure ()

gqlWarning :: GQLError -> LocalM ()
gqlWarning w = LocalM $ lift $ Success {result = (), warnings = [w]}

defaultDerivations :: [DerivingClass]
defaultDerivations = [GENERIC, SHOW, CLASS_EQ]

printClientType :: ClientTypeDefinition -> CodeGenType
printClientType ClientTypeDefinition {..} =
  CodeGenType
    { cgTypeName = clientTypeName,
      cgConstructors = clientCons,
      cgDerivations = defaultDerivations
    }
