{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Server.Deriving.Channels
  ( GetChannel (..),
  )
where

-- MORPHEUS
import Data.Morpheus.Types.Internal.AST
  ( FieldName (..),
  )
import Data.Semigroup ((<>))
import Data.Text
  ( pack,
  )
import GHC.Generics

data FieldChannel = FieldChannel String | Object [(FieldName, FieldChannel)]

data WithChannel e m a = WithChannel
  { channel :: String,
    resolver :: e -> m a
  }

class GetChannel a where
  getChannel :: a -> FieldChannel

instance {-# OVERLAPPABLE #-} (Generic a, TypeRep (Rep a)) => GetChannel a where
  getChannel = Object . typeRep . from

instance GetChannel (WithChannel e m a) where
  getChannel WithChannel {channel} = FieldChannel channel

class TypeRep f where
  typeRep :: f a -> [(FieldName, FieldChannel)]

instance TypeRep f => TypeRep (M1 D d f) where
  typeRep (M1 src) = typeRep src

instance FieldRep f => TypeRep (M1 C c f) where
  typeRep (M1 src) = fieldRep src

--- FIELDS
class FieldRep f where
  fieldRep :: f a -> [(FieldName, FieldChannel)]

instance (FieldRep f, FieldRep g) => FieldRep (f :*: g) where
  fieldRep (a :*: b) = fieldRep a <> fieldRep b

instance (Selector s, GetChannel a) => FieldRep (M1 S s (K1 s2 a)) where
  fieldRep m@(M1 (K1 src)) = [(FieldName $ pack (selName m), getChannel src)]

instance FieldRep U1 where
  fieldRep _ = []
