{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Data.Morpheus.Types.Internal.AST.SelectionMap
    ( SelectionMap
    -- , unsafeFromValues
    , traverseWithKey
    , foldWithKey
    , toOrderedMap
    , concatTraverse
    , join
    )
where 

import           Data.Set                               (Set)
import           Data.HashMap.Lazy                      ( HashMap )
import qualified Data.HashMap.Lazy                      as HM 
import           Data.Semigroup                         ((<>))
import           Data.Maybe                             (isJust,fromMaybe)
import           Language.Haskell.TH.Syntax             ( Lift(..) )

-- MORPHEUS
import           Data.Morpheus.Error.NameCollision      (NameCollision(..))
import           Data.Morpheus.Types.Internal.Operation ( Join(..)
                                                        , Empty(..)
                                                        , Singleton(..)
                                                        , Selectable(..)
                                                        , Listable(..)
                                                        , Failure(..)
                                                        , KeyOf(..)
                                                        , toPair
                                                        )
import           Data.Morpheus.Types.Internal.AST.Base  ( Name
                                                        , Named
                                                        , GQLErrors
                                                        )
import           Data.Morpheus.Types.Internal.AST.OrderedMap 
                                                        ( OrderedMap(..) )
import qualified Data.Morpheus.Types.Internal.AST.OrderedMap as OM 

-- SelectionMap 
newtype SelectionMap a = SelectionMap { 
    unSelectionMap :: OrderedMap a
  } deriving (Show, Eq, Functor)

concatTraverse :: (NameCollision b , KeyOf a, Join a, Join b, KeyOf b, Monad m, Failure GQLErrors m) => (a -> m (SelectionMap b)) -> SelectionMap a -> m (SelectionMap b)
concatTraverse f smap = traverse f (toList smap) >>= join 

join :: (NameCollision a, KeyOf a , Join a, Monad m, Failure GQLErrors m) => [SelectionMap a] -> m (SelectionMap a)
join = __join empty
 where
  __join :: (NameCollision a , KeyOf a, Join a, Monad m, Failure GQLErrors m) => SelectionMap a ->[SelectionMap a] -> m (SelectionMap a)
  __join acc [] = pure acc
  __join acc (x:xs) = acc <:> x >>= (`__join` xs)

toOrderedMap :: SelectionMap a -> OrderedMap a
toOrderedMap  = unSelectionMap

traverseWithKey :: Applicative t => (Name -> a -> t b) -> SelectionMap a -> t (SelectionMap b)
traverseWithKey f = fmap SelectionMap . OM.traverseWithKey f . unSelectionMap

foldWithKey :: (NameCollision a, KeyOf a, Join a) => (Name -> a -> b -> b) -> b -> SelectionMap a -> b
foldWithKey f defValue om = foldr (uncurry f) defValue (toAssoc om)

instance Lift a => Lift (SelectionMap a) where
  lift (SelectionMap x) = [| SelectionMap x |]

instance Foldable SelectionMap where
  foldMap f SelectionMap { unSelectionMap } = foldMap f unSelectionMap

instance Traversable SelectionMap where
  traverse f SelectionMap { unSelectionMap } = SelectionMap  <$> traverse f unSelectionMap

instance Empty (SelectionMap a) where 
  empty = SelectionMap empty

instance (KeyOf a) => Singleton (SelectionMap a) a where
  singleton = SelectionMap . singleton

instance Selectable (SelectionMap a) a where 
  selectOr fb f key  = selectOr  fb f key . unSelectionMap

-- must merge files on collision 
instance (KeyOf a, Join a) => Join (SelectionMap a) where 
  (<:>) = safeJoin

instance (KeyOf a, Join a) => Listable (SelectionMap a) a where
  fromAssoc = safeFromList
  toAssoc = toAssoc . unSelectionMap 

safeFromList :: (Monad m, KeyOf a, Join a ,Failure GQLErrors m) => [Named a] -> m (SelectionMap a)
safeFromList  = insertList empty . map snd

safeJoin :: (Monad m, KeyOf a, Join a ,Failure GQLErrors m) => SelectionMap a -> SelectionMap a -> m (SelectionMap a)
safeJoin hm1 hm2 = insertList hm1 (toList hm2)

insertList:: (Monad m, KeyOf a, Join a ,Failure GQLErrors m) =>  SelectionMap a -> [a] -> m (SelectionMap a)
insertList smap [] = pure smap
insertList smap (x:xs) = insert smap x >>= (`insertList` xs)

insert :: (Monad m, KeyOf a , Join a ,Failure GQLErrors m) => SelectionMap a -> a -> m (SelectionMap a)
insert (SelectionMap om) value = do 
  newValue <- selectOr (pure value) (<:> value) (keyOf value) om
  pure $ SelectionMap (OM.update newValue om)