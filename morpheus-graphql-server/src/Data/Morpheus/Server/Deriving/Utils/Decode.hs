{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.Decode
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
    DescribeFields (countFields),
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Morpheus.App.Internal.Resolving (ResolverState)
import Data.Morpheus.Internal.Utils
  ( selectOr,
  )
import Data.Morpheus.Server.Deriving.Utils (conNameProxy)
import Data.Morpheus.Server.Deriving.Utils.Kinded (KindedProxy (..))
import Data.Morpheus.Server.Types.GQLType
  ( GQLType,
    deriveTypename,
  )
import Data.Morpheus.Server.Types.Internal
import Data.Morpheus.Types.GQLScalar
  ( toScalar,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
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
    options :: GQLTypeOptions,
    enumVisitor :: TypeName -> TypeName,
    fieldVisitor :: FieldName -> FieldName
  }

type DecoderT = ReaderT Context ResolverState

setVariantRef :: Bool -> DecoderT a -> DecoderT a
setVariantRef isVariantRef = local (\ctx -> ctx {isVariantRef})

class DescribeCons (f :: Type -> Type) where
  tags :: Proxy f -> Context -> Info

instance (Datatype d, DescribeCons f) => DescribeCons (M1 D d f) where
  tags _ = tags (Proxy @f)

instance (DescribeCons a, DescribeCons b) => DescribeCons (a :+: b) where
  tags _ = tags (Proxy @a) <> tags (Proxy @b)

instance (Constructor c, DescribeFields a) => DescribeCons (M1 C c a) where
  tags _ Context {typeName, options} = getTag (refType (Proxy @a))
    where
      getTag (Just memberRef)
        | isUnionRef memberRef = Info {kind = VariantRef, tagName = [memberRef]}
        | otherwise = Info {kind = InlineVariant, tagName = [consName]}
      getTag Nothing = Info {kind = InlineVariant, tagName = [consName]}
      --------
      consName = conNameProxy options (Proxy @c)
      ----------
      isUnionRef x = typeName <> x == consName

getUnionInfos ::
  forall f a b.
  (DescribeCons a, DescribeCons b) =>
  f (a :+: b) ->
  DecoderT (Bool, ([TypeName], [TypeName]))
getUnionInfos _ = do
  context <- ask
  let l = tags (Proxy @a) context
  let r = tags (Proxy @b) context
  let k = kind (l <> r)
  pure (k == VariantRef, (tagName l, tagName r))

class DescribeFields (f :: Type -> Type) where
  refType :: Proxy f -> Maybe TypeName
  countFields :: Proxy f -> Int

instance (DescribeFields f, DescribeFields g) => DescribeFields (f :*: g) where
  refType _ = Nothing
  countFields _ = countFields (Proxy @f) + countFields (Proxy @g)

instance (Selector s, GQLType a) => DescribeFields (M1 S s (K1 i a)) where
  refType _ = Just $ deriveTypename (KindedProxy :: KindedProxy IN a)
  countFields _ = 1

instance DescribeFields U1 where
  refType _ = Nothing
  countFields _ = 0
