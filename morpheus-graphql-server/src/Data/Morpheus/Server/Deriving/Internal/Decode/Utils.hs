{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Internal.Decode.Utils
  ( withScalar,
    handleEither,
    DecoderT,
    setVariantRef,
    Context (..),
    getUnionInfos,
    DescribeCons,
    repValue,
    useDecodeArguments,
    coerceInputObject,
    getField,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Morpheus.App.Internal.Resolving (ResolverState)
import Data.Morpheus.Generic
  ( CountFields,
    GRepField (..),
    GRepValue (..),
    RefType (..),
  )
import Data.Morpheus.Generic.Proxy (ProxyCon (..))
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Internal.Utils
  ( fromElems,
    selectOr,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
    inputType,
  )
import Data.Morpheus.Server.Deriving.Utils.Proxy
  ( conNameProxy,
  )
import Data.Morpheus.Server.Deriving.Utils.Types (argumentsToObject)
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
    UseGQLType (..),
    UseGQLValue (..),
  )
import Data.Morpheus.Types.GQLScalar
  ( toScalar,
  )
import Data.Morpheus.Types.Internal.AST
  ( Arguments,
    CONST,
    FieldName,
    GQLError,
    IN,
    Msg (msg),
    ObjectEntry (..),
    ScalarValue,
    Token,
    TypeName,
    VALID,
    ValidObject,
    ValidValue,
    Value (..),
    internal,
  )
import GHC.Generics
import Relude

repValue :: GRepValue (GQLResult (Value CONST)) -> GQLResult (Value CONST)
repValue GRepValueEnum {..} = pure $ Enum enumVariantName
repValue GRepValueObject {..} = Object <$> (traverse fromField objectFields >>= fromElems)
  where
    fromField GRepField {fieldSelector, fieldValue} = do
      entryValue <- fieldValue
      pure ObjectEntry {entryName = fieldSelector, entryValue}
repValue _ = throwError (internal "input unions are not supported")

coerceInputObject :: (MonadError GQLError m) => ValidValue -> m ValidObject
coerceInputObject (Object object) = pure object
coerceInputObject isType = throwError (typeMismatch "InputObject" isType)

withScalar ::
  (Applicative m, MonadError GQLError m) =>
  TypeName ->
  (ScalarValue -> Either Token a) ->
  Value VALID ->
  m a
withScalar typename decodeScalar value = case toScalar value >>= decodeScalar of
  Right scalar -> pure scalar
  Left message ->
    throwError
      ( typeMismatch
          ("SCALAR(" <> msg typename <> ")" <> msg message)
          value
      )

getField :: FieldName -> ValidObject -> ValidValue
getField = selectOr Null entryValue

handleEither :: (MonadError GQLError m) => Either GQLError a -> m a
handleEither = either throwError pure

-- if value is already validated but value has different type
typeMismatch :: GQLError -> Value s -> GQLError
typeMismatch text jsType =
  internal $
    "Type mismatch! expected:"
      <> text
      <> ", got: "
      <> msg jsType

data VariantKind = InlineVariant | VariantRef deriving (Eq, Ord)

data Info = Info
  { kind :: VariantKind,
    tagName :: [TypeName]
  }

instance Semigroup Info where
  Info VariantRef t1 <> Info _ t2 = Info VariantRef (t1 <> t2)
  Info _ t1 <> Info VariantRef t2 = Info VariantRef (t1 <> t2)
  Info InlineVariant t1 <> Info InlineVariant t2 = Info InlineVariant (t1 <> t2)

data Context = Context
  { isVariantRef :: Bool,
    typeName :: TypeName,
    enumVisitor :: TypeName -> TypeName,
    fieldVisitor :: FieldName -> FieldName
  }

type DecoderT = ReaderT Context ResolverState

setVariantRef :: Bool -> DecoderT a -> DecoderT a
setVariantRef isVariantRef = local (\ctx -> ctx {isVariantRef})

class DescribeCons ctx (f :: Type -> Type) where
  tags :: ctx -> Proxy f -> Context -> Info

instance (Datatype d, DescribeCons gql f) => DescribeCons gql (M1 D d f) where
  tags ctx _ = tags ctx (Proxy @f)

instance (DescribeCons gql a, DescribeCons gql b) => DescribeCons gql (a :+: b) where
  tags ctx _ = tags ctx (Proxy @a) <> tags ctx (Proxy @b)

instance (UseGQLType ctx gql, Constructor c, CountFields a, RefType gql a) => DescribeCons ctx (M1 C c a) where
  tags ctx _ Context {typeName} = getTag (refType (Proxy @a))
    where
      getTag :: Maybe (ProxyCon gql) -> Info
      getTag (Just (ProxyCon p))
        | isUnionRef = Info {kind = VariantRef, tagName = [memberRef]}
        | otherwise = Info {kind = InlineVariant, tagName = [consName]}
        where
          memberRef = useTypename ctx (inputType p)
          isUnionRef = typeName <> memberRef == consName
      getTag Nothing = Info {kind = InlineVariant, tagName = [consName]}
      --------
      consName = conNameProxy (Proxy @c)

----------

getUnionInfos ::
  forall ctx f a b gql.
  (UseGQLType ctx gql, DescribeCons ctx a, DescribeCons ctx b) =>
  ctx ->
  f (a :+: b) ->
  DecoderT (Bool, ([TypeName], [TypeName]))
getUnionInfos ctx _ = do
  context <- ask
  let l = tags ctx (Proxy @a) context
  let r = tags ctx (Proxy @b) context
  let k = kind (l <> r)
  pure (k == VariantRef, (tagName l, tagName r))

useDecodeArguments :: (val a) => UseDeriving gql val -> Arguments VALID -> ResolverState a
useDecodeArguments ctx = useDecodeValue ctx . argumentsToObject
