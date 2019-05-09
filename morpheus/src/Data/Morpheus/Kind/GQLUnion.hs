{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Kind.GQLUnion
  ( encode
  , field
  , introspect
  , Constraint
  ) where

import           Data.Morpheus.Kind.GQLType          (GQLType (..))
import           Data.Morpheus.Schema.Internal.Types (ObjectField (..), TypeLib)
import           Data.Morpheus.Schema.TypeKind       (TypeKind (..))
import           Data.Morpheus.Schema.Utils.Utils    (Field, InputValue, Type)
import           Data.Morpheus.Types.Error           (ResolveIO, failResolveIO)
import           Data.Morpheus.Types.JSType          (JSType (..))
import           Data.Morpheus.Types.Query.Selection (Selection (..))
import           Data.Proxy
import           Data.Text                           (Text)
import           GHC.Generics

type Constraint a = (Generic a, GQLType a)

encode :: Generic a => (Text, Selection) -> a -> ResolveIO JSType
encode (_, SelectionSet _ selection _pos) _ = pure JSNull
encode (_, Field _ key pos) _               = pure JSNull

field ::
     forall a. (GQLType a)
  => Proxy a
  -> Text
  -> ObjectField
field proxy = ObjectField [] . buildField UNION proxy

introspect ::
     forall a. (GQLType a)
  => Proxy a
  -> TypeLib
  -> TypeLib
introspect _ stack = stack
