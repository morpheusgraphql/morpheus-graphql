{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Interpreting.Utils
  ( CodeGenMonad (..),
    CodeGenM,
    ServerCodeGenContext (..),
    CodeGenT,
    getFieldName,
    getEnumName,
    isParamResolverType,
    lookupFieldType,
    isSubscription,
    inType,
    runCodeGenT,
    Flags,
    Flag (..),
    langExtension,
  )
where

import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenTypeName (CodeGenTypeName),
    fromTypeName,
  )
import Data.Morpheus.CodeGen.TH
  ( ToName (toName),
  )
import Data.Morpheus.CodeGen.Utils
  ( camelCaseFieldName,
  )
import Data.Morpheus.Error (gqlWarnings)
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Internal.Utils (selectOr)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    CONST,
    DirectiveDefinition (..),
    FieldDefinition (..),
    FieldName,
    GQLError,
    OperationType (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    isResolverType,
    lookupWith,
  )
import Language.Haskell.TH
  ( Dec (..),
    Info (..),
    Q,
    TyVarBndr,
    reify,
  )
import Relude hiding (ByteString, get)

class (MonadReader ServerCodeGenContext m, Monad m, MonadFail m, CodeGenMonad m, MonadState Flags m) => CodeGenM m

instance CodeGenMonad m => CodeGenM (CodeGenT m)

instance MonadTrans CodeGenT where
  lift = CodeGenT . lift . lift

data ServerCodeGenContext = ServerCodeGenContext
  { toArgsTypeName :: FieldName -> TypeName,
    typeDefinitions :: [TypeDefinition ANY CONST],
    directiveDefinitions :: [DirectiveDefinition CONST],
    currentTypeName :: Maybe TypeName,
    currentKind :: Maybe TypeKind,
    hasNamespace :: Bool
  }

type Flags = [Flag]

newtype Flag = FlagLanguageExtension Text
  deriving (Ord, Eq)

langExtension :: MonadState Flags m => Text -> m ()
langExtension ext = modify (FlagLanguageExtension ext :)

newtype CodeGenT m a = CodeGenT
  { _runCodeGenT :: ReaderT ServerCodeGenContext (StateT Flags m) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFail,
      MonadReader ServerCodeGenContext,
      MonadState Flags
    )

runCodeGenT :: Monad m => CodeGenT m a -> ServerCodeGenContext -> m (a, Flags)
runCodeGenT (CodeGenT m) ctx = runStateT (runReaderT m ctx) mempty

getFieldName :: CodeGenM m => FieldName -> m FieldName
getFieldName fieldName = do
  ServerCodeGenContext {hasNamespace, currentTypeName} <- ask
  pure $
    if hasNamespace
      then maybe fieldName (`camelCaseFieldName` fieldName) currentTypeName
      else fieldName

getEnumName :: MonadReader ServerCodeGenContext m => TypeName -> m CodeGenTypeName
getEnumName enumName = do
  ServerCodeGenContext {hasNamespace, currentTypeName} <- ask
  pure $
    if hasNamespace
      then CodeGenTypeName (map coerce $ maybeToList currentTypeName) [] enumName
      else fromTypeName enumName

class (Monad m, MonadFail m) => CodeGenMonad m where
  isParametrizedType :: TypeName -> m Bool
  printWarnings :: [GQLError] -> m ()

instance CodeGenMonad m => CodeGenMonad (CodeGenT m) where
  isParametrizedType = lift . isParametrizedType
  printWarnings = lift . printWarnings

instance CodeGenMonad Q where
  isParametrizedType name = isParametrizedHaskellType <$> reify (toName name)
  printWarnings = gqlWarnings

instance CodeGenMonad GQLResult where
  isParametrizedType _ = pure False
  printWarnings _ = pure ()

-- Utils: is Parametrized type

#if MIN_VERSION_template_haskell(2,17,0)
getTypeVariables :: Dec -> [TyVarBndr ()]
#else
getTypeVariables :: Dec -> [TyVarBndr]
#endif
getTypeVariables (DataD _ _ args _ _ _) = args
getTypeVariables (NewtypeD _ _ args _ _ _) = args
getTypeVariables (TySynD _ args _) = args
getTypeVariables _ = []

isParametrizedHaskellType :: Info -> Bool
isParametrizedHaskellType (TyConI x) = not $ null $ getTypeVariables x
isParametrizedHaskellType _ = False

isParametrizedResolverType :: CodeGenM m => TypeName -> [TypeDefinition ANY s] -> m Bool
isParametrizedResolverType "__TypeKind" _ = pure False
isParametrizedResolverType "Boolean" _ = pure False
isParametrizedResolverType "String" _ = pure False
isParametrizedResolverType "Int" _ = pure False
isParametrizedResolverType "Float" _ = pure False
isParametrizedResolverType name lib = case lookupWith typeName name lib of
  Just x -> pure (isResolverType x)
  Nothing -> isParametrizedType name

isParamResolverType :: CodeGenM m => TypeName -> m Bool
isParamResolverType typeConName =
  isParametrizedResolverType typeConName =<< asks typeDefinitions

notFoundError :: MonadFail m => String -> String -> m a
notFoundError name at = fail $ "can't found " <> name <> "at " <> at <> "!"

lookupType :: CodeGenM m => TypeName -> m (TypeDefinition ANY CONST)
lookupType name = do
  types <- asks typeDefinitions
  case find (\t -> typeName t == name) types of
    Just x -> pure x
    Nothing -> notFoundError (show name) "type definitions"

lookupFieldType :: CodeGenM m => TypeName -> FieldName -> m TypeRef
lookupFieldType name fieldName = do
  TypeDefinition {typeContent} <- lookupType name
  case typeContent of
    DataInputObject fields -> do
      FieldDefinition {fieldType} <- selectOr (notFoundError (show fieldName) (show name)) pure fieldName fields
      pure fieldType
    _ -> notFoundError "input object" (show name)

isSubscription :: TypeKind -> Bool
isSubscription (KindObject (Just Subscription)) = True
isSubscription _ = False

inType :: MonadReader ServerCodeGenContext m => Maybe TypeName -> m a -> m a
inType name = local (\x -> x {currentTypeName = name, currentKind = Nothing})
