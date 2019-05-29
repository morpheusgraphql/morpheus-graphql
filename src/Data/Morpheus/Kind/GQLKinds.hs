{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Morpheus.Kind.GQLKinds where

import           Data.Maybe                                 (fromMaybe)
import           Data.Morpheus.Error.Internal               (internalErrorIO)
import           Data.Morpheus.Error.Selection              (subfieldsNotSelected)
import           Data.Morpheus.Generics.DeriveResolvers     (DeriveResolvers, deriveResolvers, resolveBySelection,
                                                             resolversBy)
import           Data.Morpheus.Generics.EnumRep             (EnumRep (..))
import           Data.Morpheus.Generics.GDecode             (GDecode (..))
import           Data.Morpheus.Generics.ObjectRep           (ObjectRep (..))
import           Data.Morpheus.Generics.UnionRep            (UnionRep (..))
import           Data.Morpheus.Generics.UnionResolvers      (UnionResolvers (..))
import           Data.Morpheus.Kind.GQLType                 (GQLType (..), asObjectType, enumTypeOf, inputObjectOf)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..), SelectionRec (..), SelectionSet)
import           Data.Morpheus.Types.Internal.Data          (DataFullType (..), DataInputField, DataOutputField,
                                                             DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation    (ResolveIO, Validation, failResolveIO)
import           Data.Morpheus.Types.Internal.Value         (ScalarValue (..), Value (..))
import           Data.Proxy                                 (Proxy (..))
import           Data.Text                                  (Text)
import           GHC.Generics

-- class Types class
type Intro_ a = Proxy a -> DataTypeLib -> DataTypeLib

type Decode_ a = Value -> Validation a

type Encode_ a c = (Text, Selection) -> a -> ResolveIO (Value, [c])

type IField_ a = Proxy a -> Text -> DataInputField

type OField_ a = Proxy a -> Text -> DataOutputField

type EnumConstraint a = (Generic a, EnumRep (Rep a), GQLType a)

introspectEnum ::
     forall a. (GQLType a, EnumRep (Rep a))
  => Intro_ a
introspectEnum = updateLib (enumTypeOf $ getTags (Proxy @(Rep a))) []

{-
 INPUT Object
-}
type IOObjectRep a = ObjectRep (Rep a) (Text, DataInputField)

type IObjectConstraint a = (GQLType a, Generic a, GDecode Value (Rep a), IOObjectRep a)

introspectInputObject ::
     forall a. (GQLType a, IOObjectRep a)
  => Intro_ a
introspectInputObject = updateLib (inputObjectOf fields') stack'
  where
    (fields', stack') = unzip $ getFields (Proxy @(Rep a))

{-

OBJECT

-}
type ObjectConstraint a = (Generic a, DeriveResolvers (Rep a), ObjectRep (Rep a) (Text, DataOutputField), GQLType a)

type ResolverT c = (Text, (Text, Selection) -> ResolveIO (Value, [c]))

encodeObject ::
     forall a c. (GQLType a, Generic a, DeriveResolvers (Rep a))
  => Encode_ a c
encodeObject (_, Selection {selectionRec = SelectionSet selection'}) value =
  resolveBySelection selection' ((__typename : deriveResolvers "" (from value)) :: [ResolverT c])
  where
    __typename :: (Text, (Text, Selection) -> ResolveIO (Value, [c]))
    __typename = ("__typename", const $ return (Scalar $ String $typeID (Proxy @a), []))
encodeObject (key, Selection {selectionPosition = position'}) _ = failResolveIO $ subfieldsNotSelected key "" position'

introspectObject ::
     forall a. (ObjectRep (Rep a) (Text, DataOutputField), GQLType a)
  => Intro_ a
introspectObject = updateLib (asObjectType fields') stack'
  where
    (fields', stack') = unzip $ getFields (Proxy @(Rep a))

{-

UNION

-}
type UnionConstraint a = (Generic a, GQLType a, UnionRep (Rep a), UnionResolvers (Rep a))

-- SPEC: if there is no any fragment that supports current object Type GQL returns {}
lookupSelectionByType :: Text -> [(Text, SelectionSet)] -> SelectionSet
lookupSelectionByType type' sel = fromMaybe [] $ lookup type' sel

encodeUnion ::
     forall a c. (Generic a, UnionResolvers (Rep a))
  => Encode_ a c
encodeUnion (key', sel@Selection {selectionRec = UnionSelection selections'}) value = do
  value' <- resolver (key', sel {selectionRec = SelectionSet (lookupSelectionByType type' selections')})
  return (fst value', [])
  where
    (type', resolver) = currentResolver (from value)
encodeUnion _ _ = internalErrorIO "union Resolver only should recieve UnionSelection"

introspectUnion ::
     forall a. (GQLType a, UnionRep (Rep a))
  => Intro_ a
introspectUnion = updateLib (const $ Union fields) stack
  where
    (fields, stack) = unzip $ possibleTypes (Proxy @(Rep a))
