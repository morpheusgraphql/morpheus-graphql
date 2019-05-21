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
  , introspect
  , ObjectConstraint
  ) where

import           Data.Morpheus.Error.Selection                     (subfieldsNotSelected)
import           Data.Morpheus.Generics.DeriveResolvers            (DeriveResolvers, resolveBySelection, resolversBy)
import           Data.Morpheus.Generics.ObjectRep                  (ObjectRep (..))
import           Data.Morpheus.Kind.GQLType                        (GQLType (..), asObjectType)
import           Data.Morpheus.Kind.Internal                       (KIND, OBJECT)
import           Data.Morpheus.Schema.Directive                    (Directive)
import           Data.Morpheus.Schema.EnumValue                    (EnumValue)
import           Data.Morpheus.Schema.Internal.RenderIntrospection (Field, InputValue, Type)
import           Data.Morpheus.Schema.Schema                       (Schema)
import           Data.Morpheus.Types.Internal.AST.Selection        (Selection (..))
import           Data.Morpheus.Types.Internal.Data                 (DataOutputField, DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation           (ResolveIO, failResolveIO)
import           Data.Morpheus.Types.Internal.Value                (ScalarValue (..), Value (..))
import           Data.Proxy
import           Data.Text                                         (Text)
import           GHC.Generics

type ObjectConstraint a = (Generic a, DeriveResolvers (Rep a), ObjectRep (Rep a) (Text, DataOutputField), GQLType a)

encode ::
     forall a. (GQLType a, Generic a, DeriveResolvers (Rep a))
  => (Text, Selection)
  -> a
  -> ResolveIO Value
encode (_, SelectionSet _ selection _pos) value = resolveBySelection selection (resolversBy value ++ [__typename])
  where
    __typename = ("__typename", \_ -> return $ Scalar $ String (typeID (Proxy @a)))
encode (key, SelectionField _ pos) _ = failResolveIO $ subfieldsNotSelected key "" pos -- TODO: must be internal Error

introspect ::
     forall a. (ObjectRep (Rep a) (Text, DataOutputField), GQLType a)
  => Proxy a
  -> DataTypeLib
  -> DataTypeLib
introspect = updateLib (asObjectType fields') stack'
  where
    (fields', stack') = unzip $ getFields (Proxy @(Rep a))

type instance KIND EnumValue = OBJECT

type instance KIND Type = OBJECT

type instance KIND Field = OBJECT

type instance KIND InputValue = OBJECT

type instance KIND Schema = OBJECT

type instance KIND Directive = OBJECT
