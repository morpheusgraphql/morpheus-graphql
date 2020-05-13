{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Types.Internal.Operation
  ( Empty (..),
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
  )
where

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.List (find)
import Data.Morpheus.Types.Internal.AST.Base
  ( GQLErrors,
    Name,
    Ref (..),
  )
import Instances.TH.Lift ()
import Text.Megaparsec.Internal (ParsecT (..))
import Text.Megaparsec.Stream (Stream)

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
  type KEY a = Name
  keyOf :: a -> KEY a

instance KeyOf Ref where
  keyOf = refName

toPair :: KeyOf a => a -> (KEY a, a)
toPair x = (keyOf x, x)

class Listable c a | c -> a where
  size :: c -> Int
  size = length . toList
  fromAssoc :: (Monad m, Failure GQLErrors m) => [(KEY a, a)] -> m c
  toAssoc :: c -> [(KEY a, a)]
  fromList :: (KeyOf a, Monad m, Failure GQLErrors m) => [a] -> m c

  -- TODO: fromValues
  toList = map snd . toAssoc
  fromList = fromAssoc . map toPair

  -- TODO: toValues
  toList :: c -> [a]

keys :: Listable c a => c -> [KEY a]
keys = map fst . toAssoc

class Merge a where
  (<:>) :: (Monad m, Failure GQLErrors m) => a -> a -> m a
  (<:>) = merge []
  merge :: (Monad m, Failure GQLErrors m) => [Ref] -> a -> a -> m a

class Applicative f => Failure error (f :: * -> *) where
  failure :: error -> f v

instance Failure error (Either error) where
  failure = Left

instance (Stream s, Ord e, Failure [a] m) => Failure [a] (ParsecT e s m) where
  failure x = ParsecT $ \_ _ _ _ _ -> failure x
