{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus.Resolve.Generics.TypeRep
  ( UnionRep(..)
  , ObjectRep(..)
  , TypeUpdater
  , SelOf
  , RecSel
  , resolveTypes
  ) where

import           Control.Monad                           (foldM)
import           Data.Function                           ((&))
import           Data.Morpheus.Types.Internal.Data       (DataField, DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation (SchemaValidation)
import           Data.Proxy                              (Proxy (..))
import           Data.Text                               (Text)
import           GHC.Generics

type SelOf s = M1 S s (Rec0 ()) ()

type RecSel s a = M1 S s (Rec0 a)

type TypeUpdater = DataTypeLib -> SchemaValidation DataTypeLib

{-
  introspect Union Types
-}
class UnionRep f where
  possibleTypes :: Proxy f -> [(DataField (), TypeUpdater)]

instance UnionRep f => UnionRep (M1 D x f) where
  possibleTypes _ = possibleTypes (Proxy @f)

instance UnionRep f => UnionRep (M1 C x f) where
  possibleTypes _ = possibleTypes (Proxy @f)

instance (UnionRep a, UnionRep b) => UnionRep (a :+: b) where
  possibleTypes _ = possibleTypes (Proxy @a) ++ possibleTypes (Proxy @b)

{-
  introspect Input and Output Object Types
-}
resolveTypes :: DataTypeLib -> [TypeUpdater] -> SchemaValidation DataTypeLib
resolveTypes = foldM (&)

class ObjectRep rep t where
  objectFieldTypes :: Proxy rep -> [((Text, DataField t), TypeUpdater)]

instance ObjectRep f t => ObjectRep (M1 D x f) t where
  objectFieldTypes _ = objectFieldTypes (Proxy @f)

instance ObjectRep f t => ObjectRep (M1 C x f) t where
  objectFieldTypes _ = objectFieldTypes (Proxy @f)

instance (ObjectRep a t, ObjectRep b t) => ObjectRep (a :*: b) t where
  objectFieldTypes _ = objectFieldTypes (Proxy @a) ++ objectFieldTypes (Proxy @b)

instance ObjectRep U1 t where
  objectFieldTypes _ = []
