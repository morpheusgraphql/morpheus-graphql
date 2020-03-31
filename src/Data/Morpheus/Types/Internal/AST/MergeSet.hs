{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.Morpheus.Types.Internal.AST.MergeSet
    ( MergeSet
    , toOrderedMap
    , concatTraverse
    , join
    )
where 

import           Data.List                              (find, (\\))
import           Data.Maybe                             (maybe)
import           Language.Haskell.TH.Syntax             ( Lift(..) )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Operation ( Merge(..)
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
                                                        , Ref
                                                        )
import           Data.Morpheus.Types.Internal.AST.OrderedMap 
                                                        ( OrderedMap(..) )
import qualified Data.Morpheus.Types.Internal.AST.OrderedMap as OM 

-- set with mergeable components
newtype MergeSet a = MergeSet { 
    unpack :: [a]
  } deriving ( Show, Eq, Functor, Foldable , Lift )

concatTraverse :: (KeyOf a,  Eq a, Eq b, Merge a, Merge b, KeyOf b, Monad m, Failure GQLErrors m) => (a -> m (MergeSet b)) -> MergeSet a -> m (MergeSet b)
concatTraverse f smap = traverse f (toList smap) >>= join 

join :: (Eq a, KeyOf a , Merge a, Monad m, Failure GQLErrors m) => [MergeSet a] -> m (MergeSet a)
join = __join empty
 where
  __join :: (Eq a,KeyOf a, Merge a, Monad m, Failure GQLErrors m) => MergeSet a ->[MergeSet a] -> m (MergeSet a)
  __join acc [] = pure acc
  __join acc (x:xs) = acc <:> x >>= (`__join` xs)

toOrderedMap :: KeyOf a => MergeSet a -> OrderedMap a
toOrderedMap = OM.unsafeFromValues . unpack

instance Traversable MergeSet where
  traverse f = fmap MergeSet . traverse f . unpack

instance Empty (MergeSet a) where 
  empty = MergeSet []

instance (KeyOf a) => Singleton (MergeSet a) a where
  singleton x = MergeSet [x]

instance KeyOf a => Selectable (MergeSet a) a where 
  selectOr fb _ "" _  = fb
  selectOr fb f key (MergeSet ls)  = maybe fb f (find ((key ==) . keyOf) ls)

-- must merge files on collision 
instance (KeyOf a, Merge a, Eq a) => Merge (MergeSet a) where 
  merge = safeJoin

instance (KeyOf a, Merge a, Eq a) => Listable (MergeSet a) a where
  fromAssoc = safeFromList
  toAssoc = map toPair . unpack 

safeFromList :: (Monad m, KeyOf a, Eq a, Merge a ,Failure GQLErrors m) => [Named a] -> m (MergeSet a)
safeFromList  = insertList [] empty . map snd

safeJoin :: (Monad m, KeyOf a, Eq a, Merge a ,Failure GQLErrors m) => [Ref] -> MergeSet a -> MergeSet a -> m (MergeSet a)
safeJoin path hm1 hm2 = insertList path hm1 (toList hm2)

insertList:: (Monad m, Eq a, KeyOf a, Merge a ,Failure GQLErrors m) =>  [Ref] -> MergeSet a -> [a] -> m (MergeSet a)
insertList _ smap [] = pure smap
insertList path smap (x:xs) = insert path smap x >>= flip (insertList path) xs

insert :: (Monad m, Eq a, KeyOf a , Merge a ,Failure GQLErrors m) => [Ref] -> MergeSet a -> a -> m (MergeSet a)
insert  path mSet@(MergeSet ls) currentValue = MergeSet <$> __insert
  where
    __insert = selectOr 
      (pure $ ls <> [currentValue])
      mergeWith
      (keyOf currentValue) 
      mSet
    ------------------
    mergeWith oldValue
      | oldValue == currentValue = pure ls
      | otherwise = do 
          mergedValue <- merge path oldValue currentValue
          pure $ ( ls \\ [oldValue]) <> [mergedValue]



  