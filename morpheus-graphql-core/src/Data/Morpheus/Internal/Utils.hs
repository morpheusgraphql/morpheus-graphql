{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
  )
where

import Data.Char
  ( toLower,
    toUpper,
  )
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.List (find)
import Data.Maybe (maybe)
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    FieldName (..),
    GQLErrors,
    Ref (..),
    Token,
    TypeName (..),
    TypeNameRef (..),
  )
import Data.Semigroup ((<>))
import qualified Data.Text as T
  ( concat,
    pack,
    unpack,
  )
import Instances.TH.Lift ()
import Text.Megaparsec.Internal (ParsecT (..))
import Text.Megaparsec.Stream (Stream)

mapText :: (String -> String) -> Token -> Token
mapText f = T.pack . f . T.unpack

nameSpaceType :: [FieldName] -> TypeName -> TypeName
nameSpaceType list (TypeName name) = TypeName . T.concat $ map capital (map readName list <> [name])

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

instance (Hashable k, KeyOf v, k ~ KEY v) => Collection v (HashMap k v) where
  empty = HM.empty
  singleton x = HM.singleton (keyOf x) x

class Selectable c a | c -> a where
  selectOr :: d -> (a -> d) -> KEY a -> c -> d

instance KeyOf a => Selectable [a] a where
  selectOr fb f key lib = maybe fb f (find ((key ==) . keyOf) lib)

instance (KEY a ~ k, Eq k, Hashable k) => Selectable (HashMap k a) a where
  selectOr fb f key lib = maybe fb f (HM.lookup key lib)

selectBy :: (Failure e m, Selectable c a, Monad m) => e -> KEY a -> c -> m a
selectBy err = selectOr (failure err) pure

member :: forall a c. Selectable c a => KEY a -> c -> Bool
member = selectOr False toTrue
  where
    toTrue :: a -> Bool
    toTrue _ = True

class Eq (KEY a) => KeyOf a where
  type KEY a :: *
  type KEY a = FieldName
  keyOf :: a -> KEY a

instance KeyOf Ref where
  keyOf = refName

instance KeyOf TypeNameRef where
  type KEY TypeNameRef = TypeName
  keyOf = typeNameRef

toPair :: KeyOf a => a -> (KEY a, a)
toPair x = (keyOf x, x)

-- list Like Collections
class Listable a coll | coll -> a where
  elems :: coll -> [a]
  fromElems :: (KeyOf a, Monad m, Failure GQLErrors m) => [a] -> m coll

keys :: (KeyOf a, Listable a coll) => coll -> [KEY a]
keys = map keyOf . elems

size :: Listable a coll => coll -> Int
size = length . elems

-- Merge Object with of Failure as an Option
class Merge a where
  merge :: (Monad m, Failure GQLErrors m) => [Ref] -> a -> a -> m a

(<:>) :: (Monad m, Merge a, Failure GQLErrors m) => a -> a -> m a
(<:>) = merge []

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

data Stategy t a = Disjunktive
  { disjunktiveCollection :: t a,
    nonDisjunktiveElems :: [a]
  }
  deriving (Functor)

instance
  ( Foldable t,
    KeyOf a,
    Upsert t a
  ) =>
  Semigroup (Stategy t a)
  where
  (Disjunktive x dupsX) <> (Disjunktive y dupsY) = case insertElemsWith upsert x y of
    (ta, dups) -> Disjunktive ta (dupsX <> dupsY <> dups)

class Upsert t a where
  upsert ::
    -- handle (value that stays, value that is stored as duplicate)
    (a -> a -> (Maybe a, [a])) ->
    -- value
    a ->
    -- collection
    t a ->
    -- result
    (t a, [a])

instance
  ( Eq k,
    Hashable k,
    KEY a ~ k,
    KeyOf a
  ) =>
  Upsert (HashMap k) a
  where
  upsert f el coll =
    maybe
      (HM.insert key el coll, [])
      ( mapFst
          (maybe coll (flip (HM.insert key) coll))
          . f el
      )
      (key `HM.lookup` coll)
    where
      key = keyOf el

insertElemsWith ::
  Foldable t' =>
  ( ( a ->
      a ->
      (Maybe a, [a])
    ) ->
    -- value
    a ->
    -- collection
    t a ->
    -- result
    (t a, [a])
  ) ->
  t a ->
  t' a ->
  (t a, [a])
insertElemsWith insertElem t = foldr myInsert (t, [])
  where
    myInsert x (coll, dups) =
      mapSnd (<> dups) $
        insertElem noUpdates x coll

noUpdates :: a -> a -> (Maybe a, [a])
noUpdates _ newElem = (Nothing, [newElem])

mergeElems :: (Foldable t, KeyOf a, Upsert t a) => t a -> t a -> (Maybe (Stategy t a), [t a])
mergeElems old new = (Just (Disjunktive old [] <> Disjunktive new []), [])
