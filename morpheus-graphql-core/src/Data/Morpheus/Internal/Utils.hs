{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Internal.Utils
  ( capital,
    nonCapital,
    nameSpaceField,
    nameSpaceType,
    capitalTypeName,
    Collection (..),
    Selectable (..),
    Listable (..),
    Merge (..),
    Failure (..),
    KeyOf (..),
    toPair,
    selectBy,
    member,
    keys,
    size,
    (<:>),
    mapFst,
    mapSnd,
    mapTuple,
    UpdateT (..),
    resolveUpdates,
    concatUpdates,
    failUpdates,
    ordTraverse,
    ordTraverse_,
    traverseCollection,
    (<.>),
    SemigroupM (..),
    safeUnionWith,
  )
where

import Control.Applicative (Applicative (..))
import Control.Monad ((=<<), foldM)
import Data.Char
  ( toLower,
    toUpper,
  )
import Data.Foldable (null, traverse_)
import Data.Function ((&))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.List (find)
import Data.Maybe (isJust, maybe)
import Data.Morpheus.Error.NameCollision (NameCollision (..))
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    FieldName (..),
    GQLErrors,
    Ref (..),
    Token,
    TypeName (..),
    TypeNameRef (..),
  )
import Data.Semigroup (Semigroup (..))
import qualified Data.Text as T
  ( concat,
    pack,
    unpack,
  )
import Data.Traversable (traverse)
import Instances.TH.Lift ()
import Text.Megaparsec.Internal (ParsecT (..))
import Text.Megaparsec.Stream (Stream)
import Prelude
  ( ($),
    (.),
    Bool (..),
    Either (..),
    Eq (..),
    Functor (..),
    Int,
    Monad,
    Ord,
    String,
    const,
    fst,
    length,
    otherwise,
    snd,
  )

mapText :: (String -> String) -> Token -> Token
mapText f = T.pack . f . T.unpack

nameSpaceType :: [FieldName] -> TypeName -> TypeName
nameSpaceType list (TypeName name) = TypeName . T.concat $ fmap capital (fmap readName list <> [name])

nameSpaceField :: TypeName -> FieldName -> FieldName
nameSpaceField nSpace (FieldName name) = FieldName (nonCapital nSpace <> capital name)

nonCapital :: TypeName -> Token
nonCapital = mapText __nonCapital . readTypeName
  where
    __nonCapital [] = []
    __nonCapital (x : xs) = toLower x : xs

capital :: Token -> Token
capital = mapText __capital
  where
    __capital [] = []
    __capital (x : xs) = toUpper x : xs

capitalTypeName :: FieldName -> TypeName
capitalTypeName = TypeName . capital . readName

--(KEY v ~ k) =>
class Collection a coll | coll -> a where
  empty :: coll
  singleton :: a -> coll

instance Collection a [a] where
  empty = []
  singleton x = [x]

instance KeyOf k v => Collection v (HashMap k v) where
  empty = HM.empty
  singleton x = HM.singleton (keyOf x) x

class Selectable k a c | c -> a where
  selectOr :: d -> (a -> d) -> k -> c -> d

instance KeyOf k a => Selectable k a [a] where
  selectOr fb f key lib = maybe fb f (find ((key ==) . keyOf) lib)

instance KeyOf k a => Selectable k a (HashMap k a) where
  selectOr fb f key lib = maybe fb f (HM.lookup key lib)

selectBy :: (Failure e m, Selectable k a c, Monad m) => e -> k -> c -> m a
selectBy err = selectOr (failure err) pure

member :: Selectable k a c => k -> c -> Bool
member = selectOr False toTrue
  where
    toTrue :: a -> Bool
    toTrue _ = True

ordTraverse ::
  ( Monad f,
    Failure GQLErrors f,
    KeyOf k b,
    Listable a (t a),
    Listable b (t b)
  ) =>
  (a -> f b) ->
  t a ->
  f (t b)
ordTraverse = traverseCollection

traverseCollection ::
  ( Monad f,
    KeyOf k b,
    Listable a (t a),
    Listable b (t' b),
    Failure GQLErrors f
  ) =>
  (a -> f b) ->
  t a ->
  f (t' b)
traverseCollection f a = fromElems =<< traverse f (elems a)

ordTraverse_ ::
  ( Monad f,
    Listable a (t a),
    Failure GQLErrors f
  ) =>
  (a -> f b) ->
  t a ->
  f ()
ordTraverse_ f a = traverse_ f (elems a)

class (Eq k, Hashable k) => KeyOf k a | a -> k where
  keyOf :: a -> k

instance (Eq k, Hashable k) => KeyOf k (k, a) where
  keyOf = fst

instance KeyOf FieldName Ref where
  keyOf = refName

instance KeyOf TypeName TypeNameRef where
  keyOf = typeNameRef

toPair :: KeyOf k a => a -> (k, a)
toPair x = (keyOf x, x)

-- list Like Collections
class Listable a coll | coll -> a where
  elems :: coll -> [a]
  fromElems :: (Monad m, Failure GQLErrors m) => [a] -> m coll

instance (NameCollision a, KeyOf k a) => Listable a (HashMap k a) where
  fromElems = safeFromList
  elems = HM.elems

keys :: (KeyOf k a, Listable a coll) => coll -> [k]
keys = fmap keyOf . elems

size :: Listable a coll => coll -> Int
size = length . elems

-- Merge Object with of Failure as an Option
class Merge a where
  merge :: (Monad m, Failure GQLErrors m) => [Ref] -> a -> a -> m a

instance (NameCollision a, KeyOf k a) => Merge (HashMap k a) where
  merge _ = safeJoin

(<:>) :: (Monad m, Merge a, Failure GQLErrors m) => a -> a -> m a
(<:>) = merge []

class SemigroupM m a where
  mergeM :: [Ref] -> a -> a -> m a

(<.>) ::
  (SemigroupM m a) =>
  a ->
  a ->
  m a
(<.>) = mergeM []

-- Failure: for custome Morpheus GrapHQL errors
class Applicative f => Failure error (f :: * -> *) where
  failure :: error -> f v

instance Failure error (Either error) where
  failure = Left

instance (Stream s, Ord e, Failure [a] m) => Failure [a] (ParsecT e s m) where
  failure x = ParsecT $ \_ _ _ _ _ -> failure x

mapFst :: (a -> a') -> (a, b) -> (a', b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> b') -> (a, b) -> (a, b')
mapSnd f (a, b) = (a, f b)

mapTuple :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
mapTuple f1 f2 (a, b) = (f1 a, f2 b)

-- Helper Functions
newtype UpdateT m a = UpdateT {updateTState :: a -> m a}

failUpdates :: (Failure e m) => e -> UpdateT m a
failUpdates = UpdateT . const . failure

concatUpdates :: Monad m => [UpdateT m a] -> UpdateT m a
concatUpdates x = UpdateT (`resolveUpdates` x)

resolveUpdates :: Monad m => a -> [UpdateT m a] -> m a
resolveUpdates a = foldM (&) a . fmap updateTState

safeFromList ::
  ( Failure GQLErrors m,
    Applicative m,
    NameCollision a,
    Eq k,
    Hashable k,
    KeyOf k a
  ) =>
  [a] ->
  m (HashMap k a)
safeFromList values = safeUnionWith HM.empty (fmap toPair values)

safeJoin :: (Failure GQLErrors m, Eq k, Hashable k, Applicative m, NameCollision a) => HashMap k a -> HashMap k a -> m (HashMap k a)
safeJoin hm newls = safeUnionWith hm (HM.toList newls)

safeUnionWith ::
  ( Failure GQLErrors m,
    Applicative m,
    Eq k,
    Hashable k,
    NameCollision a
  ) =>
  HashMap k a ->
  [(k, a)] ->
  m (HashMap k a)
safeUnionWith hm names = case insertNoDups (hm, []) names of
  (res, dupps)
    | null dupps -> pure res
    | otherwise -> failure $ fmap (nameCollision . snd) dupps

type NoDupHashMap k a = (HashMap k a, [(k, a)])

insertNoDups :: (Eq k, Hashable k) => NoDupHashMap k a -> [(k, a)] -> NoDupHashMap k a
insertNoDups collected [] = collected
insertNoDups (coll, errors) (pair@(name, value) : xs)
  | isJust (name `HM.lookup` coll) = insertNoDups (coll, errors <> [pair]) xs
  | otherwise = insertNoDups (HM.insert name value coll, errors) xs
