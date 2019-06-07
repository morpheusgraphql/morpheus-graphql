{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Resolve.Introspect where

import           Data.Morpheus.Kind                     (ENUM, KIND, OBJECT, SCALAR, UNION, WRAPPER)
import           Data.Morpheus.Resolve.Generics.TypeRep (ObjectRep (..), RecSel, SelOf, UnionRep (..), resolveTypes)
import           Data.Morpheus.Resolve.Internal         (EnumConstraint, Intro_, OField_, ObjectConstraint,
                                                         UnionConstraint, introspectEnum, listField, maybeField)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import qualified Data.Morpheus.Types.GQLArgs            as Args (GQLArgs (..))
import qualified Data.Morpheus.Types.GQLScalar          as S (GQLScalar (..))
import           Data.Morpheus.Types.GQLType            (GQLType (..), asObjectType)
import           Data.Morpheus.Types.Internal.Data      (DataField (..), DataFullType (..), DataOutputField)
import           Data.Morpheus.Types.Resolver           (Resolver (..))
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text, pack)
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
  __objectField :: Proxy kind -> OField_ a
  __introspect :: Proxy kind -> Intro_ a

{--

  Introspect SCALAR Types: SCALAR, ENUM

-}
instance (S.GQLScalar a, GQLType a) => Introspect a SCALAR where
  __objectField _ _ = field_ SCALAR (Proxy @a) []
  __introspect _ _ = S.introspect (Proxy @a)

instance EnumConstraint a => Introspect a ENUM where
  __objectField _ _ = field_ ENUM (Proxy @a) []
  __introspect _ _ = introspectEnum (Proxy @a)

{--

  Introspect OBJECT Types: OBJECTS, UNIONS

-}
instance (Selector s, Introspect a (KIND a)) => ObjectRep (RecSel s a) (Text, DataOutputField) where
  getFields _ = [((name, _objectField (Proxy @a) name), _introspect (Proxy @a))]
    where
      name = pack $ selName (undefined :: SelOf s)

instance ObjectConstraint a => Introspect a OBJECT where
  __objectField _ _ = field_ OBJECT (Proxy @a) []
  __introspect _ = updateLib (asObjectType fields') stack'
    where
      (fields', stack') = unzip $ getFields (Proxy @(Rep a))

instance (Introspect a OBJECT, ObjectConstraint a) => UnionRep (RecSel s a) where
  possibleTypes _ = [(field_ OBJECT (Proxy @a) () "", __introspect (Proxy @OBJECT) (Proxy @a))]

instance UnionConstraint a => Introspect a UNION where
  __objectField _ _ = field_ UNION (Proxy @a) []
  __introspect _ = updateLib (Union . buildType fields) stack
    where
      (fields, stack) = unzip $ possibleTypes (Proxy @(Rep a))

{--

  Introspect WRAPPER Types: Maybe, LIST , Resolver

-}
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
