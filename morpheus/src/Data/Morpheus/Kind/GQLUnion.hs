{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Kind.GQLUnion
  ( encode
  , introspect
  , Constraint
  ) where

import           Data.Maybe                            (fromMaybe)
import           Data.Morpheus.Error.Internal          (internalErrorIO)
import           Data.Morpheus.Generics.UnionRep       (UnionRep (..))
import           Data.Morpheus.Generics.UnionResolvers (UnionResolvers (..))
import           Data.Morpheus.Kind.GQLType            (GQLType (..))
import           Data.Morpheus.Schema.Internal.AST     (LibType (..), TypeLib)
import           Data.Morpheus.Types.Error             (ResolveIO)
import           Data.Morpheus.Types.JSType            (JSType (..))
import           Data.Morpheus.Types.Query.Selection   (Selection (..), SelectionSet)
import           Data.Proxy
import           Data.Text                             (Text)
import           GHC.Generics

type Constraint a = (Generic a, GQLType a, UnionRep (Rep a), UnionResolvers (Rep a))

-- SPEC: if there is no any fragment that supports current object Type GQL returns {}
lookupSelectionByType :: Text -> [(Text, SelectionSet)] -> SelectionSet
lookupSelectionByType type' sel = fromMaybe [] $ lookup type' sel

encode :: (Generic a, UnionResolvers (Rep a)) => (Text, Selection) -> a -> ResolveIO JSType
encode (key', UnionSelection args selection pos) value =
  resolver (key', SelectionSet args (lookupSelectionByType type' selection) pos)
  where
    (type', resolver) = currentResolver (from value)
encode _ _ = internalErrorIO "union Resolver only should recieve UnionSelection"

introspect ::
     forall a. (GQLType a, UnionRep (Rep a))
  => Proxy a
  -> TypeLib
  -> TypeLib
introspect = updateLib (const $ Union fields) stack
  where
    fieldTypes = possibleTypes (Proxy @(Rep a))
    fields = map fst fieldTypes
    stack = map snd fieldTypes
