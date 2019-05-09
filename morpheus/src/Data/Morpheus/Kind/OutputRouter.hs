{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Kind.OutputRouter where

import           Control.Monad.Trans                    (lift)
import           Control.Monad.Trans.Except
import           Data.Morpheus.Error.Selection          (fieldNotResolved)
import           Data.Morpheus.Generics.DeriveResolvers (DeriveResolvers (..))
import           Data.Morpheus.Generics.ObjectRep       (ObjectRep (..), resolveTypes)
import           Data.Morpheus.Generics.Utils           (RecSel, SelOf)
import qualified Data.Morpheus.Kind.GQLArgs             as Args (GQLArgs (..))
import qualified Data.Morpheus.Kind.GQLEnum             as E (EnumConstraint, encode, field, introspect)
import qualified Data.Morpheus.Kind.GQLObject           as O (ObjectConstraint, encode, field, introspect)
import qualified Data.Morpheus.Kind.GQLScalar           as S (GQLScalar (..))
import           Data.Morpheus.Kind.GQLType             (GQLType)
import qualified Data.Morpheus.Kind.GQLUnion            as U (Constraint, encode, field, introspect)
import           Data.Morpheus.Kind.Internal            (ENUM, Encode_, Intro_, KIND, OBJECT, OField_, SCALAR, UNION,
                                                         WRAPPER)
import           Data.Morpheus.Kind.Utils               (encodeList, encodeMaybe, listField, maybeField)
import           Data.Morpheus.Schema.Internal.Types    (ObjectField (..))
import           Data.Morpheus.Types.Describer          ((::->) (..))
import           Data.Morpheus.Types.Error              (ResolveIO, failResolveIO)
import           Data.Morpheus.Types.Query.Selection    (Selection (..))
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text, pack)
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
  __encode _ _ = pure . S.encode
  __objectField _ _ = ObjectField [] . S.asField (Proxy @a)

instance E.EnumConstraint a => OutputTypeRouter a ENUM where
  __introspect _ _ = E.introspect (Proxy @a)
  __encode _ _ = pure . E.encode
  __objectField _ _ = ObjectField [] . E.field (Proxy @a)

instance O.ObjectConstraint a => OutputTypeRouter a OBJECT where
  __encode _ = O.encode
  __introspect _ = O.introspect
  __objectField _ = O.field

instance OutputTypeRouter a (KIND a) => DeriveResolvers (K1 s a) where
  deriveResolvers key' (K1 src) = [(key', (`_encode` src))]

instance (Selector s, OutputTypeRouter a (KIND a)) => ObjectRep (RecSel s a) (Text, ObjectField) where
  getFields _ = [((name, _objectField (Proxy @a) name), _introspect (Proxy @a))]
    where
      name = pack $ selName (undefined :: SelOf s)

instance U.Constraint a => OutputTypeRouter a UNION where
  __encode _ = U.encode
  __introspect _ = U.introspect
  __objectField _ = U.field

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

instance (OutputTypeRouter a (KIND a), Args.GQLArgs p) => OutputTypeRouter (p ::-> a) WRAPPER where
  __encode _ (key', SelectionSet gqlArgs body position') (Resolver resolver) =
    (ExceptT $ pure $ Args.decode gqlArgs) >>= liftResolver position' key' . resolver >>=
    _encode (key', SelectionSet gqlArgs body position')
  __encode _ (key', Field gqlArgs field position') (Resolver resolver) =
    (ExceptT $ pure $ Args.decode gqlArgs) >>= liftResolver position' key' . resolver >>=
    _encode (key', Field gqlArgs field position')
  __introspect _ _ typeLib = resolveTypes typeLib $ inputTypes' ++ [_introspect (Proxy @a)]
    where
      inputTypes' = map snd $ Args.introspect (Proxy @p)
  __objectField _ _ name = (_objectField (Proxy @a) name) {args = map fst $ Args.introspect (Proxy @p)}
