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
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Internal.Decode.Utils
  ( withInputObject,
    withEnum,
    withInputUnion,
    decodeFieldWith,
    withScalar,
    handleEither,
    getFieldName,
    DecoderT,
    setVariantRef,
    Context (..),
    getUnionInfos,
    DescribeCons,
    RefType (..),
    repValue,
    useDecodeArguments,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Morpheus.App.Internal.Resolving (ResolverState)
import Data.Morpheus.Generic
  ( CountFields,
    GRepField (..),
    GRepValue (..),
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Internal.Utils
  ( fromElems,
    selectOr,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
  )
import Data.Morpheus.Server.Deriving.Utils.Proxy
  ( conNameProxy,
  )
import Data.Morpheus.Server.Deriving.Utils.Types (argumentsToObject)
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
    UseGQLType (..),
    UseValue (..),
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
    getInputUnionValue,
    internal,
  )
import GHC.Generics
import Relude

repValue ::
  GRepValue (GQLResult (Value CONST)) ->
  GQLResult (Value CONST)
repValue GRepValueEnum {..} = pure $ Enum enumVariantName
repValue GRepValueObject {..} = Object <$> (traverse fromField objectFields >>= fromElems)
  where
    fromField GRepField {fieldSelector, fieldValue} = do
      entryValue <- fieldValue
      pure ObjectEntry {entryName = fieldSelector, entryValue}
repValue _ = throwError (internal "input unions are not supported")

withInputObject ::
  MonadError GQLError m =>
  (ValidObject -> m a) ->
  ValidValue ->
  m a
withInputObject f (Object object) = f object
withInputObject _ isType = throwError (typeMismatch "InputObject" isType)

-- | Useful for more restrictive instances of lists (non empty, size indexed etc)
withEnum :: MonadError GQLError m => (TypeName -> m a) -> Value VALID -> m a
withEnum decode (Enum value) = decode value
withEnum _ isType = throwError (typeMismatch "Enum" isType)

withInputUnion ::
  (MonadError GQLError m, Monad m) =>
  (TypeName -> ValidObject -> ValidObject -> m a) ->
  ValidObject ->
  m a
withInputUnion decoder unions =
  either onFail onSuccess (getInputUnionValue unions)
  where
    onSuccess (name, value) = withInputObject (decoder name unions) value
    onFail = throwError . internal . msg

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

decodeFieldWith :: (Value VALID -> m a) -> FieldName -> ValidObject -> m a
decodeFieldWith decoder = selectOr (decoder Null) (decoder . entryValue)

handleEither :: MonadError GQLError m => Either GQLError a -> m a
handleEither = either throwError pure

-- if value is already validated but value has different type
typeMismatch :: GQLError -> Value s -> GQLError
typeMismatch text jsType =
  internal $
    "Type mismatch! expected:"
      <> text
      <> ", got: "
      <> msg jsType

getFieldName :: FieldName -> Int -> FieldName
getFieldName "" index = "_" <> show index
getFieldName label _ = label

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

class DescribeCons gql (f :: Type -> Type) where
  tags :: UseGQLType gql -> Proxy f -> Context -> Info

instance (Datatype d, DescribeCons gql f) => DescribeCons gql (M1 D d f) where
  tags ctx _ = tags ctx (Proxy @f)

instance (DescribeCons gql a, DescribeCons gql b) => DescribeCons gql (a :+: b) where
  tags ctx _ = tags ctx (Proxy @a) <> tags ctx (Proxy @b)

instance (Constructor c, CountFields a, RefType gql a) => DescribeCons gql (M1 C c a) where
  tags ctx _ Context {typeName} = getTag (refType ctx (Proxy @a))
    where
      getTag (Just memberRef)
        | isUnionRef memberRef = Info {kind = VariantRef, tagName = [memberRef]}
        | otherwise = Info {kind = InlineVariant, tagName = [consName]}
      getTag Nothing = Info {kind = InlineVariant, tagName = [consName]}
      --------
      consName = conNameProxy (Proxy @c)
      ----------
      isUnionRef x = typeName <> x == consName

getUnionInfos ::
  forall f a b gql.
  (DescribeCons gql a, DescribeCons gql b) =>
  UseGQLType gql ->
  f (a :+: b) ->
  DecoderT (Bool, ([TypeName], [TypeName]))
getUnionInfos ctx _ = do
  context <- ask
  let l = tags ctx (Proxy @a) context
  let r = tags ctx (Proxy @b) context
  let k = kind (l <> r)
  pure (k == VariantRef, (tagName l, tagName r))

class RefType gql (f :: Type -> Type) where
  refType :: UseGQLType gql -> Proxy f -> Maybe TypeName

instance (RefType gql f, RefType gql g) => RefType gql (f :*: g) where
  refType _ _ = Nothing

instance (Selector s, gql a) => RefType gql (M1 S s (K1 i a)) where
  refType dir _ = Just $ useTypename dir (InputType :: CatType IN a)

instance RefType gql U1 where
  refType _ _ = Nothing

useDecodeArguments :: val a => UseDeriving gql val -> Arguments VALID -> ResolverState a
useDecodeArguments drv = useDecodeValue (drvArgs drv) . argumentsToObject
