{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import qualified Data.Map as M
import Data.Morpheus.App.Internal.Resolving
  ( ResolverState,
  )
import Data.Morpheus.Internal.Ext
  ( GQLResult,
  )
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
import Data.Morpheus.Server.Deriving.Utils.GRep
  ( DeriveWith,
    DerivingOptions (..),
    deriveValue,
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
    UseValue (..),
  )
import Data.Morpheus.Server.Types.Kind
  ( CUSTOM,
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

class KindedValue gql args (kind :: DerivingKind) (a :: Type) where
  encodeKindedValue :: UseDeriving gql args -> ContextValue kind a -> GQLResult (Value CONST)
  decodeKindedValue :: UseDeriving gql args -> Proxy kind -> ValidValue -> ResolverState a

instance (EncodeScalar a, DecodeScalar a, gql a) => KindedValue gql args SCALAR a where
  encodeKindedValue _ = pure . Scalar . encodeScalar . unContextValue
  decodeKindedValue dir _ = withScalar (useTypename (dirGQL dir) (InputType :: CatType IN a)) decodeScalar

instance (DecodeWrapperConstraint f a, DecodeWrapper f, EncodeWrapperValue f, args a) => KindedValue gql args WRAPPER (f a) where
  encodeKindedValue dir = encodeWrapperValue (useEncodeValue (dirArgs dir)) . unContextValue
  decodeKindedValue dir _ value =
    runExceptT (decodeWrapper (useDecodeValue (dirArgs dir)) value)
      >>= handleEither

instance (gql a, Generic a, DecodeRep gql args (Rep a), DeriveWith gql args (GQLResult (Value CONST)) (Rep a)) => KindedValue gql args TYPE a where
  encodeKindedValue UseDeriving {..} =
    repValue
      . deriveValue
        ( DerivingOptions
            { optApply = useEncodeValue dirArgs . runIdentity,
              optTypeData = useTypeData dirGQL . inputType
            } ::
            DerivingOptions gql args Identity (GQLResult (Value CONST))
        )
      . unContextValue
  decodeKindedValue dir _ = fmap to . (`runReaderT` context) . decodeRep dir
    where
      context =
        Context
          { isVariantRef = False,
            typeName = useTypename (dirGQL dir) (InputType :: CatType IN a),
            enumVisitor = visitEnumName dir proxy,
            fieldVisitor = visitFieldName dir proxy
          }
        where
          proxy = Proxy @a

instance KindedValue gql args CUSTOM (Value CONST) where
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

instance (KnownSymbol name, args a) => KindedValue gql args CUSTOM (Arg name a) where
  encodeKindedValue _ _ = throwError "directives cant be tagged arguments"
  decodeKindedValue UseDeriving {dirArgs} _ value = Arg <$> withInputObject fieldDecoder value
    where
      fieldDecoder = decodeFieldWith (useDecodeValue dirArgs) fieldName
      fieldName = symbolName (Proxy @name)

--  Map
instance (Ord k, val [(k, v)]) => KindedValue gql val CUSTOM (Map k v) where
  decodeKindedValue dir _ v = M.fromList <$> (useDecodeValue (dirArgs dir) v :: ResolverState [(k, v)])
  encodeKindedValue dir = useEncodeValue (dirArgs dir) . M.toList . unContextValue
