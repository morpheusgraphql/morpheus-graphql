{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Execution.Server.Encode
  ( EncodeCon
  , EncodeMutCon
  , EncodeSubCon
  , encodeQuery
  , encodeMut
  , encodeSub
  ) where

import           Control.Monad                                   ((>=>))

--import           Control.Monad.Trans.Except
import           Control.Monad.Except                            (ExceptT (..), liftEither, runExceptT, withExceptT)

import           Data.Map                                        (Map)
import qualified Data.Map                                        as M (toList)
import           Data.Maybe                                      (fromMaybe)
import           Data.Proxy                                      (Proxy (..))
import           Data.Set                                        (Set)
import qualified Data.Set                                        as S (toList)
import           Data.Text                                       (Text, pack)
import           Data.Typeable                                   (Typeable)
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Error.Internal                    (internalErrorT)
import           Data.Morpheus.Error.Selection                   (resolverError, subfieldsNotSelected)
import           Data.Morpheus.Execution.Server.Decode           (DecodeObject, decodeArguments)
import           Data.Morpheus.Execution.Server.Generics.EnumRep (EnumRep (..))
import           Data.Morpheus.Kind                              (ENUM, GQL_KIND, OBJECT, SCALAR, UNION)
import           Data.Morpheus.Types.Custom                      (MapKind, Pair (..), mapKindFromList)
import           Data.Morpheus.Types.GQLScalar                   (GQLScalar (..))
import           Data.Morpheus.Types.GQLType                     (GQLType (KIND, __typeName))
import           Data.Morpheus.Types.Internal.AST.Operation      (Operation (..), ValidOperation)
import           Data.Morpheus.Types.Internal.AST.Selection      (Selection (..), SelectionRec (..), SelectionSet)
import           Data.Morpheus.Types.Internal.Stream             (PublishStream, StreamT (..), SubscribeStream,
                                                                  initExceptStream, injectEvents)
import           Data.Morpheus.Types.Internal.Validation         (GQLErrors, ResolveT, failResolveT)
import           Data.Morpheus.Types.Internal.Value              (ScalarValue (..), Value (..))
import           Data.Morpheus.Types.Resolver                    (Event (..), Resolver, SubResolveT, SubResolver (..))

class Encode a result where
  encode :: a -> (Text, Selection) -> result

instance {-# OVERLAPPABLE #-} EncodeKind (KIND a) a res => Encode a res where
  encode resolver = encodeKind (ResKind resolver :: ResKind (KIND a) a)

-- MAYBE
instance (DefaultValue res, Encode a res) => Encode (Maybe a) res where
  encode Nothing      = const (nullValue :: res)
  encode (Just value) = encode value

--  Tuple  (a,b)
instance Encode (Pair k v) result => Encode (k, v) result where
  encode (key, value) = encode (Pair key value)

--  Set
instance Encode [a] result => Encode (Set a) result where
  encode = encode . S.toList

--  Map
instance (Eq k, Monad m, Encode (MapKind k v (Resolver m)) (ResolveT m res)) => Encode (Map k v) (ResolveT m res) where
  encode value = encode ((mapKindFromList $ M.toList value) :: MapKind k v (Resolver m))

-- LIST []
instance (Monad m, DefaultValue res, Encode a (m res)) => Encode [a] (m res) where
  encode list query = listValue <$> mapM (`encode` query) list

-- GQL Either Resolver
instance (Monad m, Encode a (ResolveT m res), DecodeObject p) => Encode (p -> Either String a) (ResolveT m res) where
  encode resolver selection = decodeArgs selection >>= encodeResolver selection . (ExceptT . pure . resolver)

--  GQL ExceptT Resolver
instance (DecodeObject a, Monad m, Encode b (ResolveT m res)) => Encode (a -> Resolver m b) (ResolveT m res) where
  encode resolver selection = decodeArgs selection >>= encodeResolver selection . resolver

-- GQL Mutation Resolver
instance (DecodeObject a, Monad m, Encode b (ResolveT m res)) =>
         Encode (a -> Resolver m b) (ResolveT (StreamT m c) res) where
  encode resolver = injectEvents [] . encode resolver

-- GQL Subscription Resolver
instance (DecodeObject a, Monad m, Encode b (ResolveT m Value)) =>
         Encode (a -> SubResolver m e c b) (SubResolveT m e c Value) where
  encode resolver selection = decodeArgs selection >>= handleResolver . resolver
    where
      handleResolver SubResolver {subChannels, subResolver} =
        initExceptStream [subChannels] (encodeResolver selection . subResolver)

-- ENCODE GQL KIND
class EncodeKind (kind :: GQL_KIND) a result where
  encodeKind :: ResKind kind a -> (Text, Selection) -> result

-- SCALAR
instance (GQLScalar a, Monad m) => EncodeKind SCALAR a (m Value) where
  encodeKind = pure . pure . Scalar . serialize . unResKind

-- ENUM
instance (EnumConstraint a, Monad m) => EncodeKind ENUM a (m Value) where
  encodeKind = pure . pure . Scalar . String . encodeRep . from . unResKind

--  OBJECTS
instance (GQLType a, DefaultValue res, ResConstraint a m res) => EncodeKind OBJECT a (ResolveT m res) where
  encodeKind (ResKind value) (_, Selection {selectionRec = SelectionSet selection}) =
    resolveBySelection selection (__typenameResolver : resolversBy value)
    where
      __typenameResolver = ("__typename", const $ return $ stringValue $ __typeName (Proxy @a))
  encodeKind _ (key, Selection {selectionPosition}) = failResolveT $ subfieldsNotSelected key "" selectionPosition

-- UNION,
instance ResConstraint a m res => EncodeKind UNION a (ResolveT m res) where
  encodeKind (ResKind value) (key, sel@Selection {selectionRec = UnionSelection selections}) =
    resolver (key, sel {selectionRec = SelectionSet lookupSelection})
      -- SPEC: if there is no any fragment that supports current object Type GQL returns {}
    where
      lookupSelection = fromMaybe [] $ lookup typeName selections
      (typeName, resolver) = unionResolvers (from value)
  encodeKind _ _ = internalErrorT "union Resolver only should recieve UnionSelection"

-- Types & Constrains -------------------------------------------------------
type EncodeOperator m a value = Resolver m a -> ValidOperation -> m (Either GQLErrors value)

type EncodeCon m a v = (Generic a, Typeable a, GResolver (Rep a) (ResolveT m v))

type EncodeMutCon m event con mut = EncodeCon (PublishStream m event con) mut Value

type EncodeSubCon m event con sub = EncodeCon (SubscribeStream m event) sub (Event event con -> ResolveT m Value)

type ResConstraint a m res = (Monad m, Generic a, GResolver (Rep a) (ResolveT m res))

type EnumConstraint a = (Generic a, EnumRep (Rep a))

type FieldRes m res = (Text, (Text, Selection) -> ResolveT m res)

newtype ResKind (kind :: GQL_KIND) a = ResKind
  { unResKind :: a
  }

--- GENERICS ------------------------------------------------
--  OBJECT
-- | Derives resolvers by object fields
class GResolver f result where
  fieldResolvers :: f a -> [(Text, (Text, Selection) -> result)]
  unionResolvers :: f a -> (Text, (Text, Selection) -> result)

instance GResolver U1 res where
  fieldResolvers _ = []

instance (Selector s, GQLType a, Encode a res) => GResolver (M1 S s (K1 s2 a)) res where
  fieldResolvers m@(M1 (K1 src)) = [(pack $ selName m, encode src)]
  unionResolvers (M1 (K1 src)) = (__typeName (Proxy @a), encode src)

instance GResolver f res => GResolver (M1 D c f) res where
  fieldResolvers (M1 src) = fieldResolvers src
  unionResolvers (M1 x) = unionResolvers x

instance GResolver f res => GResolver (M1 C c f) res where
  fieldResolvers (M1 src) = fieldResolvers src
  unionResolvers (M1 x) = unionResolvers x

instance (GResolver f res, GResolver g res) => GResolver (f :*: g) res where
  fieldResolvers (a :*: b) = fieldResolvers a ++ fieldResolvers b

instance (GResolver a res, GResolver b res) => GResolver (a :+: b) res where
  unionResolvers (L1 x) = unionResolvers x
  unionResolvers (R1 x) = unionResolvers x

----- HELPERS ----------------------------
encodeQuery :: (Monad m, EncodeCon m schema Value, EncodeCon m a Value) => schema -> EncodeOperator m a Value
encodeQuery types rootResolver operator@Operation {operationSelection} =
  runExceptT
    (fmap resolversBy (operatorToResolveT operator rootResolver) >>=
     resolveBySelection operationSelection . (++) (resolversBy types))

encodeMut :: (Monad m, EncodeCon m a res, DefaultValue res) => EncodeOperator m a res
encodeMut = encodeOperator resolveBySelection

encodeSub ::
     (Monad m, EncodeSubCon m event con a)
  => EncodeOperator (SubscribeStream m event) a (Event event con -> ResolveT m Value)
encodeSub = encodeOperator (flip resolveSelection)
  where
    resolveSelection resolvers = fmap toObj . traverse (selectResolver resolvers)
      where
        toObj pairs args = objectValue <$> traverse keyVal pairs
          where
            keyVal (key, valFunc) = (key, ) <$> valFunc args

encodeResolver :: (Monad m, Encode a (ResolveT m res)) => (Text, Selection) -> Resolver m a -> ResolveT m res
encodeResolver selection@(fieldName, Selection {selectionPosition}) =
  withExceptT (resolverError selectionPosition fieldName) >=> (`encode` selection)

encodeOperator ::
     (Monad m, EncodeCon m a res) => (SelectionSet -> [FieldRes m res] -> ResolveT m res) -> EncodeOperator m a res
encodeOperator resSel rootResolver operation@Operation {operationSelection} =
  runExceptT (operatorToResolveT operation rootResolver >>= resSel operationSelection . resolversBy)

operatorToResolveT :: Monad m => ValidOperation -> Resolver m a -> ResolveT m a
operatorToResolveT Operation {operationPosition, operationName} =
  withExceptT (resolverError operationPosition operationName)

decodeArgs :: (Monad m, DecodeObject a) => (Text, Selection) -> ResolveT m a
decodeArgs = liftEither . decodeArguments . selectionArguments . snd

resolveBySelection :: (Monad m, DefaultValue res) => SelectionSet -> [FieldRes m res] -> ResolveT m res
resolveBySelection = flip (resolveGroup objectValue)

resolveGroup :: (Monad m, DefaultValue a) => ([(Text, a)] -> b) -> [FieldRes m a] -> SelectionSet -> ResolveT m b
resolveGroup toObj resolvers = fmap toObj . traverse (selectResolver resolvers)

selectResolver :: (Monad m, DefaultValue a) => [FieldRes m a] -> (Text, Selection) -> ResolveT m (Text, a)
selectResolver resolvers (key, selection) =
  case selectionRec selection of
    SelectionAlias name selectionRec -> (key, ) <$> lookupRes name (selection {selectionRec})
    _                                -> (key, ) <$> lookupRes key selection
    -------------------------------------------------------------
  where
    lookupRes resKey sel = (fromMaybe (const $ return nullValue) $ lookup resKey resolvers) (key, sel)

resolversBy :: (Generic a, GResolver (Rep a) result) => a -> [(Text, (Text, Selection) -> result)]
resolversBy = fieldResolvers . from

--------------------------------------------
class DefaultValue a where
  nullValue :: a
  listValue :: [a] -> a
  stringValue :: Text -> a
  objectValue :: [(Text, a)] -> a

instance DefaultValue Value where
  nullValue = Null
  listValue = List
  stringValue = Scalar . String
  objectValue = Object

instance DefaultValue b => DefaultValue (a -> b) where
  nullValue = const nullValue
  stringValue = const . stringValue

instance Monad m => DefaultValue (m Value) where
  nullValue = pure nullValue
  listValue = pure nullValue
  stringValue = pure . stringValue
