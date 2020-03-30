{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveLift                 #-}

module Data.Morpheus.Types.Internal.AST.SelectionMap
    ( SelectionMap
    , toOrderedMap
    , concatTraverse
    , join
    )
where 

import           Data.List                              (find, (\\))
import           Data.Maybe                             (maybe)
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
import           Data.Morpheus.Types.Internal.AST.Base  ( Named
                                                        , GQLErrors
                                                        )
import           Data.Morpheus.Types.Internal.AST.OrderedMap 
                                                        ( OrderedMap(..) )
import qualified Data.Morpheus.Types.Internal.AST.OrderedMap as OM 

-- SelectionMap 
newtype SelectionMap a = SelectionMap { 
    unSelectionMap :: [a]
  } deriving ( Show, Eq, Functor, Foldable , Lift )

concatTraverse :: (NameCollision b , KeyOf a,  Eq a, Eq b, Join a, Join b, KeyOf b, Monad m, Failure GQLErrors m) => (a -> m (SelectionMap b)) -> SelectionMap a -> m (SelectionMap b)
concatTraverse f smap = traverse f (toList smap) >>= join 

join :: (NameCollision a, Eq a, KeyOf a , Join a, Monad m, Failure GQLErrors m) => [SelectionMap a] -> m (SelectionMap a)
join = __join empty
 where
  __join :: (NameCollision a,  Eq a,KeyOf a, Join a, Monad m, Failure GQLErrors m) => SelectionMap a ->[SelectionMap a] -> m (SelectionMap a)
  __join acc [] = pure acc
  __join acc (x:xs) = acc <:> x >>= (`__join` xs)

toOrderedMap :: KeyOf a => SelectionMap a -> OrderedMap a
toOrderedMap = OM.unsafeFromValues . unSelectionMap

instance Traversable SelectionMap where
  traverse f SelectionMap { unSelectionMap } = SelectionMap  <$> traverse f unSelectionMap

instance Empty (SelectionMap a) where 
  empty = SelectionMap []

instance (KeyOf a) => Singleton (SelectionMap a) a where
  singleton x = SelectionMap [x]

instance KeyOf a => Selectable (SelectionMap a) a where 
  selectOr fb f key (SelectionMap ls)  = maybe fb f (find ((key ==) . keyOf) ls)

-- must merge files on collision 
instance (KeyOf a, Join a, Eq a) => Join (SelectionMap a) where 
  (<:>) = safeJoin

instance (KeyOf a, Join a, Eq a) => Listable (SelectionMap a) a where
  fromAssoc = safeFromList
  toAssoc = map toPair . unSelectionMap 

safeFromList :: (Monad m, KeyOf a, Eq a, Join a ,Failure GQLErrors m) => [Named a] -> m (SelectionMap a)
safeFromList  = insertList empty . map snd

safeJoin :: (Monad m, KeyOf a, Eq a, Join a ,Failure GQLErrors m) => SelectionMap a -> SelectionMap a -> m (SelectionMap a)
safeJoin hm1 hm2 = insertList hm1 (toList hm2)

insertList:: (Monad m, Eq a, KeyOf a, Join a ,Failure GQLErrors m) =>  SelectionMap a -> [a] -> m (SelectionMap a)
insertList smap [] = pure smap
insertList smap (x:xs) = insert smap x >>= (`insertList` xs)

insert :: (Monad m, Eq a, KeyOf a , Join a ,Failure GQLErrors m) => SelectionMap a -> a -> m (SelectionMap a)
insert sm@(SelectionMap ls) value = do 
  let oldValue = selectOr value id (keyOf value) sm
  newValue <- oldValue <:> value
  pure $ SelectionMap $ (ls \\ [oldValue]) <> [newValue]