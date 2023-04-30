{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Kinded.Value
  ( KindedValue (..),
  )
where

import Control.Monad.Except
  ( MonadError (throwError),
  )
import Data.Morpheus.App.Internal.Resolving
  ( ResolverState,
  )
import Data.Morpheus.Generic
  ( GRep,
    GRepContext (..),
    deriveValue,
  )
import Data.Morpheus.Internal.Ext
  ( GQLResult,
    unsafeFromList,
  )
import Data.Morpheus.Internal.Utils (IsMap (toAssoc))
import Data.Morpheus.Server.Deriving.Internal.Decode.Rep
  ( DecodeRep (..),
  )
import Data.Morpheus.Server.Deriving.Internal.Decode.Utils
  ( Context (..),
    decodeFieldWith,
    handleEither,
    repValue,
    withInputObject,
    withScalar,
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.Directive
  ( visitEnumName,
    visitFieldName,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
    inputType,
  )
import Data.Morpheus.Server.Deriving.Utils.Proxy
  ( ContextValue,
    symbolName,
    unContextValue,
  )
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
    UseGQLType (..),
    UseGQLValue (..),
  )
import Data.Morpheus.Server.Types.Kind
  ( CUSTOM,
    DIRECTIVE,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Server.Types.Types (Arg (Arg))
import Data.Morpheus.Types.GQLScalar
  ( DecodeScalar (..),
    EncodeScalar (..),
  )
import Data.Morpheus.Types.GQLWrapper
  ( DecodeWrapper (..),
    DecodeWrapperConstraint,
    EncodeWrapperValue (encodeWrapperValue),
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    IN,
    ObjectEntry (..),
    VALID,
    ValidValue,
    Value (..),
  )
import GHC.Generics
import GHC.TypeLits (KnownSymbol)
import Relude

class KindedValue ctx (k :: DerivingKind) (a :: Type) where
  encodeKindedValue :: (UseDeriving gql args ~ ctx) => ctx -> ContextValue k a -> GQLResult (Value CONST)
  decodeKindedValue :: (UseDeriving gql args ~ ctx) => ctx -> Proxy k -> ValidValue -> ResolverState a

instance (EncodeScalar a, DecodeScalar a, ctx ~ UseDeriving gql args, gql a) => KindedValue ctx SCALAR a where
  encodeKindedValue _ = pure . Scalar . encodeScalar . unContextValue
  decodeKindedValue ctx _ = withScalar (useTypename ctx (InputType :: CatType IN a)) decodeScalar

instance (ctx ~ UseDeriving gql args, DecodeWrapperConstraint f a, DecodeWrapper f, EncodeWrapperValue f, args a) => KindedValue ctx WRAPPER (f a) where
  encodeKindedValue ctx = encodeWrapperValue (useEncodeValue ctx) . unContextValue
  decodeKindedValue ctx _ value =
    runExceptT (decodeWrapper (useDecodeValue ctx) value)
      >>= handleEither

instance (ctx ~ UseDeriving gql args, gql a, Generic a, DecodeRep ctx (Rep a), GRep gql args (GQLResult (Value CONST)) (Rep a)) => KindedValue ctx TYPE a where
  encodeKindedValue ctx =
    repValue
      . deriveValue
        ( GRepContext
            { optFun = useEncodeValue ctx . runIdentity,
              optTypename = useTypename ctx . inputType,
              optWrappers = useWrappers ctx . inputType
            } ::
            GRepContext gql args Identity (GQLResult (Value CONST))
        )
      . unContextValue
  decodeKindedValue ctx _ = fmap to . (`runReaderT` context) . decodeRep ctx
    where
      context =
        Context
          { isVariantRef = False,
            typeName = useTypename ctx (InputType :: CatType IN a),
            enumVisitor = visitEnumName ctx proxy,
            fieldVisitor = visitFieldName ctx proxy
          }
        where
          proxy = Proxy @a

instance (ctx ~ UseDeriving gql args, gql a, Generic a, DecodeRep ctx (Rep a), GRep gql args (GQLResult (Value CONST)) (Rep a)) => KindedValue ctx DIRECTIVE a where
  encodeKindedValue ctx =
    repValue
      . deriveValue
        ( GRepContext
            { optFun = useEncodeValue ctx . runIdentity,
              optTypename = useTypename ctx . inputType,
              optWrappers = useWrappers ctx . inputType
            } ::
            GRepContext gql args Identity (GQLResult (Value CONST))
        )
      . unContextValue
  decodeKindedValue ctx _ = fmap to . (`runReaderT` context) . decodeRep ctx
    where
      context =
        Context
          { isVariantRef = False,
            typeName = useTypename ctx (InputType :: CatType IN a),
            enumVisitor = visitEnumName ctx proxy,
            fieldVisitor = visitFieldName ctx proxy
          }
        where
          proxy = Proxy @a

instance KindedValue ctx CUSTOM (Value CONST) where
  encodeKindedValue _ = pure . unContextValue
  decodeKindedValue _ _ = pure . toConstValue

toConstValue :: ValidValue -> Value CONST
toConstValue Null = Null
toConstValue (Enum x) = Enum x
toConstValue (Scalar x) = Scalar x
toConstValue (List xs) = List (map toConstValue xs)
toConstValue (Object fields) = Object (fmap toEntry fields)
  where
    toEntry :: ObjectEntry VALID -> ObjectEntry CONST
    toEntry ObjectEntry {..} = ObjectEntry {entryValue = toConstValue entryValue, ..}

instance (ctx ~ UseDeriving gql args, KnownSymbol name, args a) => KindedValue ctx CUSTOM (Arg name a) where
  encodeKindedValue _ _ = throwError "directives cant be tagged arguments"
  decodeKindedValue ctx _ value = Arg <$> withInputObject fieldDecoder value
    where
      fieldDecoder = decodeFieldWith (useDecodeValue ctx) fieldName
      fieldName = symbolName (Proxy @name)

--  Map
instance (ctx ~ UseDeriving gql args, Ord k, args [(k, v)]) => KindedValue ctx CUSTOM (Map k v) where
  decodeKindedValue ctx _ v = unsafeFromList <$> (useDecodeValue ctx v :: ResolverState [(k, v)])
  encodeKindedValue ctx = useEncodeValue ctx . toAssoc . unContextValue
