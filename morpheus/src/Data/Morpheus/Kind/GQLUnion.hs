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

import           Data.Morpheus.Generics.UnionRep     (UnionRep (..))
import           Data.Morpheus.Kind.GQLType          (GQLType (..))
import           Data.Morpheus.Schema.Internal.AST   (LibType (..), TypeLib)
import           Data.Morpheus.Types.Error           (ResolveIO)
import           Data.Morpheus.Types.JSType          (JSType (..), ScalarValue (..))
import           Data.Morpheus.Types.Query.Selection (Selection (..))
import           Data.Proxy
import           Data.Text                           (Text, pack)
import           Debug.Trace
import           GHC.Generics

type Constraint a = (Generic a, GQLType a, UnionRep (Rep a))

encode :: Generic a => (Text, Selection) -> a -> ResolveIO JSType
encode (_, UnionSelection _ selection _pos) _ = pure $ Scalar $ String (trace (show selection) (pack (show selection)))
encode (_, _) _ = pure $ Scalar $ String "ERROR"

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
