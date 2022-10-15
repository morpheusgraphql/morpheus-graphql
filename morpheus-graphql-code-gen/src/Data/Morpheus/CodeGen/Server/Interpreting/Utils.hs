{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Interpreting.Utils
  ( CodeGenMonad (..),
    TypeContext (..),
    CodeGenT,
    getFieldName,
    getEnumName,
    isParamResolverType,
    lookupFieldType,
    isSubscription,
    inType,
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

type CodeGenT m = ReaderT (TypeContext CONST) m

data TypeContext s = TypeContext
  { toArgsTypeName :: FieldName -> TypeName,
    typeDefinitions :: [TypeDefinition ANY s],
    directiveDefinitions :: [DirectiveDefinition s],
    currentTypeName :: Maybe TypeName,
    currentKind :: Maybe TypeKind,
    hasNamespace :: Bool
  }

getFieldName :: Monad m => FieldName -> CodeGenT m FieldName
getFieldName fieldName = do
  TypeContext {hasNamespace, currentTypeName} <- ask
  pure $
    if hasNamespace
      then maybe fieldName (`camelCaseFieldName` fieldName) currentTypeName
      else fieldName

getEnumName :: MonadReader (TypeContext s) m => TypeName -> m CodeGenTypeName
getEnumName enumName = do
  TypeContext {hasNamespace, currentTypeName} <- ask
  pure $
    if hasNamespace
      then CodeGenTypeName (map coerce $ maybeToList currentTypeName) [] enumName
      else fromTypeName enumName

class (Monad m, MonadFail m) => CodeGenMonad m where
  isParametrizedType :: TypeName -> m Bool
  printWarnings :: [GQLError] -> m ()

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

isParametrizedResolverType :: CodeGenMonad m => TypeName -> [TypeDefinition ANY s] -> CodeGenT m Bool
isParametrizedResolverType "__TypeKind" _ = pure False
isParametrizedResolverType "Boolean" _ = pure False
isParametrizedResolverType "String" _ = pure False
isParametrizedResolverType "Int" _ = pure False
isParametrizedResolverType "Float" _ = pure False
isParametrizedResolverType name lib = case lookupWith typeName name lib of
  Just x -> pure (isResolverType x)
  Nothing -> lift (isParametrizedType name)

isParamResolverType :: CodeGenMonad m => TypeName -> ReaderT (TypeContext CONST) m Bool
isParamResolverType typeConName =
  isParametrizedResolverType typeConName =<< asks typeDefinitions

notFoundError :: MonadFail m => String -> String -> m a
notFoundError name at = fail $ "can't found " <> name <> "at " <> at <> "!"

lookupType :: MonadFail m => TypeName -> CodeGenT m (TypeDefinition ANY CONST)
lookupType name = do
  types <- asks typeDefinitions
  case find (\t -> typeName t == name) types of
    Just x -> pure x
    Nothing -> notFoundError (show name) "type definitions"

lookupFieldType :: MonadFail m => TypeName -> FieldName -> CodeGenT m TypeRef
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

inType :: MonadReader (TypeContext s) m => Maybe TypeName -> m a -> m a
inType name = local (\x -> x {currentTypeName = name, currentKind = Nothing})
