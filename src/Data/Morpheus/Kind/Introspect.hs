{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Kind.Introspect where

import           Data.Morpheus.Generics.ObjectRep  (ObjectRep (..), resolveTypes)
import           Data.Morpheus.Generics.UnionRep   (UnionRep (..))
import           Data.Morpheus.Generics.Utils      (RecSel, SelOf)
import           Data.Morpheus.Kind.GQLKinds       (EnumConstraint, Intro_, OField_, ObjectConstraint, UnionConstraint,
                                                    introspectEnum, introspectObject, introspectUnion)
import           Data.Morpheus.Kind.Internal       (ENUM, KIND, OBJECT, SCALAR, UNION, WRAPPER)
import           Data.Morpheus.Kind.Utils          (listField, maybeField)
import           Data.Morpheus.Schema.TypeKind     (TypeKind (..))
import qualified Data.Morpheus.Types.GQLArgs       as Args (GQLArgs (..))
import qualified Data.Morpheus.Types.GQLScalar     as S (GQLScalar (..))
import           Data.Morpheus.Types.GQLType       (GQLType (..))
import           Data.Morpheus.Types.Internal.Data (DataField (..), DataOutputField)
import           Data.Morpheus.Types.Resolver      (Resolver (..))
import           Data.Proxy                        (Proxy (..))
import           Data.Text                         (Text, pack)
import           GHC.Generics

_objectField ::
     forall a. Introspect a (KIND a)
  => OField_ a
_objectField = __objectField (Proxy @(KIND a))

_introspect ::
     forall a. Introspect a (KIND a)
  => Intro_ a
_introspect = __introspect (Proxy @(KIND a))

class Introspect a kind where
  __introspect :: Proxy kind -> Intro_ a
  __objectField :: Proxy kind -> OField_ a

instance (S.GQLScalar a, GQLType a) => Introspect a SCALAR where
  __introspect _ _ = S.introspect (Proxy @a)
  __objectField _ _ = field_ SCALAR (Proxy @a) []

instance EnumConstraint a => Introspect a ENUM where
  __introspect _ _ = introspectEnum (Proxy @a)
  __objectField _ _ = field_ ENUM (Proxy @a) []

instance (Selector s, Introspect a (KIND a)) => ObjectRep (RecSel s a) (Text, DataOutputField) where
  getFields _ = [((name, _objectField (Proxy @a) name), _introspect (Proxy @a))]
    where
      name = pack $ selName (undefined :: SelOf s)

instance ObjectConstraint a => Introspect a OBJECT where
  __introspect _ = introspectObject
  __objectField _ _ = field_ OBJECT (Proxy @a) []

instance (Introspect a OBJECT, ObjectConstraint a) => UnionRep (RecSel s a) where
  possibleTypes _ = [(field_ OBJECT (Proxy @a) () "", introspectObject (Proxy @a))]

instance UnionConstraint a => Introspect a UNION where
  __introspect _ _ = introspectUnion (Proxy @a)
  __objectField _ _ = field_ UNION (Proxy @a) []

instance Introspect a (KIND a) => Introspect (Maybe a) WRAPPER where
  __introspect _ _ = _introspect (Proxy @a)
  __objectField _ _ name = maybeField (_objectField (Proxy @a) name)

instance Introspect a (KIND a) => Introspect [a] WRAPPER where
  __introspect _ _ = _introspect (Proxy @a)
  __objectField _ _ name = listField (_objectField (Proxy @a) name)

instance (Introspect a (KIND a), Args.GQLArgs p) => Introspect (Resolver c p a) WRAPPER where
  __introspect _ _ typeLib = resolveTypes typeLib $ inputTypes' ++ [_introspect (Proxy @a)]
    where
      inputTypes' = map snd $ Args.introspect (Proxy @p)
  __objectField _ _ name = (_objectField (Proxy @a) name) {fieldArgs = map fst $ Args.introspect (Proxy @p)}
