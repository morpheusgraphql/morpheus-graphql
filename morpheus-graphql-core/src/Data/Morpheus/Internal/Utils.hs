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
    Empty (..),
    Selectable (..),
    Singleton (..),
    Listable (..),
    Merge (..),
    Failure (..),
    KeyOf (..),
    toPair,
    selectBy,
    member,
    keys,
    size,
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

class Empty a where
  empty :: a

instance Empty (HashMap k v) where
  empty = HM.empty

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

class KeyOf a => Singleton c a | c -> a where
  singleton :: a -> c

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
class Listable c a | c -> a where
  elems :: Listable c a => c -> [a]
  fromElems :: (KeyOf a, Monad m, Failure GQLErrors m) => [a] -> m c

keys :: (KeyOf a, Listable c a) => c -> [KEY a]
keys = map keyOf . elems

size :: Listable c a => c -> Int
size = length . elems

-- Merge Object with of Failure as an Option
class Merge a where
  (<:>) :: (Monad m, Failure GQLErrors m) => a -> a -> m a
  (<:>) = merge []
  merge :: (Monad m, Failure GQLErrors m) => [Ref] -> a -> a -> m a

-- Failure: for custome Morpheus GrapHQL errors
class Applicative f => Failure error (f :: * -> *) where
  failure :: error -> f v

instance Failure error (Either error) where
  failure = Left

instance (Stream s, Ord e, Failure [a] m) => Failure [a] (ParsecT e s m) where
  failure x = ParsecT $ \_ _ _ _ _ -> failure x
