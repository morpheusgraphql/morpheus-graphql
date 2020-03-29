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
import qualified Data.Morpheus.Types.Internal.AST.OrderedMap as OM 
                                                        ( OrderedMap(..)
                                                        , traverseWithKey
                                                        )

-- SelectionMap 
newtype SelectionMap a = SelectionMap { 
    unSelectionMap :: OM.OrderedMap a
  } deriving (Show, Functor)

concatTraverse :: (NameCollision b , Monad m, Failure GQLErrors m) => (a -> m (SelectionMap b)) -> SelectionMap a -> m (SelectionMap b)
concatTraverse f smap = traverse f (toList smap) >>= join 

join :: (NameCollision a , Monad m, Failure GQLErrors m) => [SelectionMap a] -> m (SelectionMap a)
join = __join empty
 where
  __join :: (NameCollision a , Monad m, Failure GQLErrors m) => SelectionMap a ->[SelectionMap a] -> m (SelectionMap a)
  __join acc [] = pure acc
  __join acc (x:xs) = acc <:> x >>= (`__join` xs)

toOrderedMap :: SelectionMap a -> OM.OrderedMap a
toOrderedMap  = unSelectionMap

traverseWithKey :: Applicative t => (Name -> a -> t b) -> SelectionMap a -> t (SelectionMap b)
traverseWithKey f = fmap SelectionMap . OM.traverseWithKey f . unSelectionMap

foldWithKey :: NameCollision a => (Name -> a -> b -> b) -> b -> SelectionMap a -> b
foldWithKey f defValue om = foldr (uncurry f) defValue (toAssoc om)

instance Lift a => Lift (SelectionMap a) where
  lift (SelectionMap x) = [| SelectionMap x |]

instance Foldable SelectionMap where
  foldMap f SelectionMap { unSelectionMap } = foldMap f unSelectionMap

instance Traversable SelectionMap where
  traverse f SelectionMap { unSelectionMap } = SelectionMap  <$> traverse f unSelectionMap

instance NameCollision a => Join (SelectionMap a) where 
  (SelectionMap k1) <:> (SelectionMap k2) = SelectionMap <$> (k1 <:> k2)

instance Empty (SelectionMap a) where 
  empty = SelectionMap empty

instance (KeyOf a) => Singleton (SelectionMap a) a where
  singleton = SelectionMap . singleton

instance Selectable (SelectionMap a) a where 
  selectOr fb f key  = selectOr  fb f key . unSelectionMap

instance Listable (SelectionMap a) a where
  fromAssoc = fmap SelectionMap . fromAssoc
  toAssoc SelectionMap {  unSelectionMap } = toAssoc unSelectionMap 

-- safeFromList :: (Failure GQLErrors m, Applicative m, NameCollision a) => [Named a] -> m (SelectionMap a)
-- safeFromList values = SelectionMap (map fst values) <$> safeUnionWith HM.empty values 

-- unsafeFromValues :: KeyOf a => [a] -> SelectionMap a
-- unsafeFromValues x = SelectionMap (map keyOf x) $ HM.fromList $ map toPair x

-- safeJoin :: (Failure GQLErrors m, Applicative m, NameCollision a) => HashMap Name a -> HashMap Name a -> m (HashMap Name a)
-- safeJoin hm newls = safeUnionWith hm (HM.toList newls)

-- safeUnionWith :: (Failure GQLErrors m, Applicative m, NameCollision a) => HashMap Name a -> [Named a] -> m (HashMap Name a)
-- safeUnionWith hm names = case insertNoDups (hm,[]) names of 
--   (res,dupps) | null dupps -> pure res
--               | otherwise -> failure $ map (uncurry nameCollision) dupps

type NoDupHashMap a = (HashMap Name a,[Named a])

insertNoDups :: NoDupHashMap a -> [Named a] -> NoDupHashMap a
insertNoDups collected [] = collected
insertNoDups (coll,errors) (pair@(name,value):xs)
  | isJust (name `HM.lookup` coll) = insertNoDups (coll,errors <> [pair]) xs
  | otherwise = insertNoDups (HM.insert name value coll,errors) xs
