{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Client.Transform.Core
  ( Converter (..),
    compileError,
    getType,
    leafType,
    typeFrom,
    deprecationWarning,
    customScalarTypes,
  )
where

--
-- MORPHEUS
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
  ( ReaderT (..),
  )
import Data.Morpheus.Error
  ( deprecatedField,
    globalErrorMessage,
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
    TypeD,
    TypeDefinition (..),
    TypeName,
    VALID,
    VariableDefinitions,
    hsTypeName,
    isSystemTypeName,
    lookupDeprecated,
    lookupDeprecatedReason,
    msg,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
  )
import Data.Semigroup ((<>))

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

compileError :: Message -> GQLErrors
compileError x =
  globalErrorMessage $ "Unhandled Compile Time Error: \"" <> x <> "\" ;"

getType :: TypeName -> Converter (TypeDefinition ANY)
getType typename = asks fst >>= selectBy (compileError $ " cant find Type" <> msg typename) typename

customScalarTypes :: TypeName -> [TypeName]
customScalarTypes typeName
  | not (isSystemTypeName typeName) = [typeName]
  | otherwise = []

leafType :: TypeDefinition a -> Converter ([TypeD cat], [TypeName])
leafType TypeDefinition {typeName, typeContent} = fromKind typeContent
  where
    fromKind :: TypeContent TRUE a -> Converter ([TypeD cat], [TypeName])
    fromKind DataEnum {} = pure ([], [typeName])
    fromKind DataScalar {} = pure ([], customScalarTypes typeName)
    fromKind _ = failure $ compileError "Invalid schema Expected scalar"

typeFrom :: [FieldName] -> TypeDefinition a -> TypeName
typeFrom path TypeDefinition {typeName, typeContent} = __typeFrom typeContent
  where
    __typeFrom DataScalar {} = hsTypeName typeName
    __typeFrom DataObject {} = nameSpaceType path typeName
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
