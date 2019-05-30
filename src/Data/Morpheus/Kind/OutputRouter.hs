{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Kind.OutputRouter where

import           Control.Monad.Trans                        (lift)
import           Control.Monad.Trans.Except
import           Data.Morpheus.Error.Selection              (fieldNotResolved)
import           Data.Morpheus.Generics.DeriveResolvers     (DeriveResolvers (..))
import           Data.Morpheus.Generics.EnumRep             (EnumRep (..))
import           Data.Morpheus.Generics.ObjectRep           (ObjectRep (..), resolveTypes)
import           Data.Morpheus.Generics.UnionRep            (UnionRep (..))
import           Data.Morpheus.Generics.UnionResolvers      (UnionResolvers (..))
import           Data.Morpheus.Generics.Utils               (RecSel, SelOf)
import qualified Data.Morpheus.Kind.GQLArgs                 as Args (GQLArgs (..))
import           Data.Morpheus.Kind.GQLKinds                (Encode_, EnumConstraint, Intro_, OField_, ObjectConstraint,
                                                             UnionConstraint, encodeObject, encodeUnion, introspectEnum,
                                                             introspectObject, introspectUnion)
import qualified Data.Morpheus.Kind.GQLScalar               as S (GQLScalar (..))
import           Data.Morpheus.Kind.GQLType                 (GQLType (..))
import           Data.Morpheus.Kind.Internal                (ENUM, KIND, OBJECT, SCALAR, UNION, WRAPPER)
import           Data.Morpheus.Kind.Utils                   (encodeList, encodeMaybe, listField, maybeField)
import           Data.Morpheus.Schema.TypeKind              (TypeKind (..))
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..))
import           Data.Morpheus.Types.Internal.Data          (DataField (..), DataOutputField)
import           Data.Morpheus.Types.Internal.Validation    (ResolveIO, failResolveIO)
import           Data.Morpheus.Types.Internal.Value         (ScalarValue (..), Value (..))
import           Data.Morpheus.Types.Resolver               (Resolver (..), Result (..))
import           Data.Proxy                                 (Proxy (..))
import           Data.Text                                  (Text, pack)
import           GHC.Generics

class OutputTypeRouter a b where
  __introspect :: Proxy b -> Intro_ a
  __encode :: Proxy b -> Encode_ a
  __objectField :: Proxy b -> OField_ a

_objectField ::
     forall a. OutputTypeRouter a (KIND a)
  => OField_ a
_objectField = __objectField (Proxy @(KIND a))

_introspect ::
     forall a. OutputTypeRouter a (KIND a)
  => Intro_ a
_introspect = __introspect (Proxy @(KIND a))

_encode ::
     forall a. OutputTypeRouter a (KIND a)
  => Encode_ a
_encode = __encode (Proxy @(KIND a))

instance (S.GQLScalar a, GQLType a) => OutputTypeRouter a SCALAR where
  __introspect _ _ = S.introspect (Proxy @a)
  __encode _ _ = pure . pure . S.encode
  __objectField _ _ = field_ SCALAR (Proxy @a) []

instance EnumConstraint a => OutputTypeRouter a ENUM where
  __introspect _ _ = introspectEnum (Proxy @a)
  __encode _ _ = pure . pure . Scalar . String . encodeRep . from
  __objectField _ _ = field_ ENUM (Proxy @a) []

instance ObjectConstraint a => OutputTypeRouter a OBJECT where
  __encode _ = encodeObject
  __introspect _ = introspectObject
  __objectField _ _ = field_ OBJECT (Proxy @a) []

instance OutputTypeRouter a (KIND a) => DeriveResolvers (K1 s a) where
  deriveResolvers key' (K1 src) = [(key', (`_encode` src))]

instance (Selector s, OutputTypeRouter a (KIND a)) => ObjectRep (RecSel s a) (Text, DataOutputField) where
  getFields _ = [((name, _objectField (Proxy @a) name), _introspect (Proxy @a))]
    where
      name = pack $ selName (undefined :: SelOf s)

instance (OutputTypeRouter a OBJECT, ObjectConstraint a) => UnionRep (RecSel s a) where
  possibleTypes _ = [(field_ OBJECT (Proxy @a) () "", introspectObject (Proxy @a))]

instance (GQLType a, OutputTypeRouter a (KIND a)) => UnionResolvers (K1 s a) where
  currentResolver (K1 src) = (typeID (Proxy @a), (`_encode` src))

instance UnionConstraint a => OutputTypeRouter a UNION where
  __encode _ = encodeUnion
  __introspect _ _ = introspectUnion (Proxy @a)
  __objectField _ _ = field_ UNION (Proxy @a) []

instance OutputTypeRouter a (KIND a) => OutputTypeRouter (Maybe a) WRAPPER where
  __encode _ = encodeMaybe _encode
  __introspect _ _ = _introspect (Proxy @a)
  __objectField _ _ name = maybeField (_objectField (Proxy @a) name)

instance OutputTypeRouter a (KIND a) => OutputTypeRouter [a] WRAPPER where
  __encode _ = encodeList _encode
  __introspect _ _ = _introspect (Proxy @a)
  __objectField _ _ name = listField (_objectField (Proxy @a) name)

liftResolver :: Int -> Text -> IO (Either String a) -> ResolveIO a
liftResolver position' typeName' x = do
  result <- lift x
  case result of
    Left message' -> failResolveIO $ fieldNotResolved position' typeName' (pack message')
    Right value   -> pure value

instance (OutputTypeRouter a (KIND a), Args.GQLArgs p) => OutputTypeRouter (Resolver c p a) WRAPPER where
  __encode _ selection'@(key', Selection {selectionArguments = astArgs', selectionPosition = position'}) (Resolver resolver) = do
    args <- ExceptT $ pure $ Args.decode astArgs'
    Result value1 effects1 <- liftResolver position' key' (resolver args)
    Result value2 effects2 <- _encode selection' value1
    return $ Result value2 (effects1 ++ effects2)
  __introspect _ _ typeLib = resolveTypes typeLib $ inputTypes' ++ [_introspect (Proxy @a)]
    where
      inputTypes' = map snd $ Args.introspect (Proxy @p)
  __objectField _ _ name = (_objectField (Proxy @a) name) {fieldArgs = map fst $ Args.introspect (Proxy @p)}
