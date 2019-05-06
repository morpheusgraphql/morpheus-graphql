{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Kind.GQLObject
  ( encode
  , field
  , introspect
  ) where

import           Data.Morpheus.Error.Selection          (subfieldsNotSelected)
import           Data.Morpheus.Generics.DeriveResolvers (DeriveResolvers (..), resolveBySelection)
import           Data.Morpheus.Generics.ObjectRep       (ObjectRep (..))
import           Data.Morpheus.Generics.Utils           (RecSel, SelOf)
import           Data.Morpheus.Kind.GQLKind             (GQLKind (..), asObjectType)
import           Data.Morpheus.Kind.Internal            (GQL, OBJECT)
import           Data.Morpheus.Kind.OutputRouter        (OutputTypeRouter (..), _encode, _introspect, _objectField)
import           Data.Morpheus.Schema.Directive         (Directive)
import           Data.Morpheus.Schema.EnumValue         (EnumValue)
import           Data.Morpheus.Schema.Internal.Types    (ObjectField (..), TypeLib)
import           Data.Morpheus.Schema.Schema            (Schema)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Morpheus.Schema.Utils.Utils       (Field, InputValue, Type)
import           Data.Morpheus.Types.Error              (ResolveIO, failResolveIO)
import           Data.Morpheus.Types.JSType             (JSType (..))
import           Data.Morpheus.Types.Query.Selection    (Selection (..))
import           Data.Proxy
import           Data.Text                              (Text, pack)
import           GHC.Generics

type ObjectConstraint a = (Generic a, DeriveResolvers (Rep a), ObjectRep (Rep a) (Text, ObjectField), GQLKind a)

instance ObjectConstraint a => OutputTypeRouter a OBJECT where
  __encode _ = encode
  __introspect _ = introspect
  __objectField _ = field

instance OutputTypeRouter a (GQL a) => DeriveResolvers (K1 s a) where
  deriveResolvers key' (K1 src) = [(key', (`_encode` src))]

instance (Selector s, OutputTypeRouter a (GQL a)) => ObjectRep (RecSel s a) (Text, ObjectField) where
  getFields _ = [((name, _objectField (Proxy @a) name), _introspect (Proxy @a))]
    where
      name = pack $ selName (undefined :: SelOf s)

encode :: (Generic a, DeriveResolvers (Rep a)) => (Text, Selection) -> a -> ResolveIO JSType
encode (_, SelectionSet _ selection _pos) = resolveBySelection selection . deriveResolvers "" . from
encode (_, Field _ key pos)               = const $ failResolveIO $ subfieldsNotSelected key "" pos -- TODO: must be internal Error

field ::
     forall a. (ObjectRep (Rep a) (Text, ObjectField), GQLKind a)
  => Proxy a
  -> Text
  -> ObjectField
field proxy = ObjectField [] . buildField OBJECT proxy

introspect ::
     forall a. (ObjectRep (Rep a) (Text, ObjectField), GQLKind a)
  => Proxy a
  -> TypeLib
  -> TypeLib
introspect = updateLib (asObjectType fields) stack
  where
    fieldTypes = getFields (Proxy @(Rep a))
    fields = map fst fieldTypes
    stack = map snd fieldTypes

type instance GQL EnumValue = OBJECT

type instance GQL Type = OBJECT

type instance GQL Field = OBJECT

type instance GQL InputValue = OBJECT

type instance GQL Schema = OBJECT

type instance GQL Directive = OBJECT
