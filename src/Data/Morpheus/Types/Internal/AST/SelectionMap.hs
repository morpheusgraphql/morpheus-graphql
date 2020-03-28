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
    , unsafeFromValues
    , traverseWithKey
    , foldWithKey
    , toOrderedMap
    )
where 

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
                                                        ( OrderedMap(..))


-- SelectionMap 
data SelectionMap a = SelectionMap { 
    mapKeys :: [Name], 
    mapEntries :: HashMap Name a 
  } deriving (Show, Functor)

toOrderedMap :: SelectionMap a -> OM.OrderedMap a
toOrderedMap (SelectionMap name entries) = OM.OrderedMap name entries

traverseWithKey :: Applicative t => (Name -> a -> t b) -> SelectionMap a -> t (SelectionMap b)
traverseWithKey f (SelectionMap names hmap) = SelectionMap names <$> HM.traverseWithKey f hmap

foldWithKey :: NameCollision a => (Name -> a -> b -> b) -> b -> SelectionMap a -> b
foldWithKey f defValue om = foldr (uncurry f) defValue (toAssoc om)

instance Lift a => Lift (SelectionMap a) where
  lift (SelectionMap names x) = [| SelectionMap names (HM.fromList ls) |]
    where ls = HM.toList x

instance Foldable SelectionMap where
  foldMap f SelectionMap { mapEntries } = foldMap f mapEntries

instance Traversable SelectionMap where
  traverse f (SelectionMap names values) = SelectionMap names <$> traverse f values

instance NameCollision a => Join (SelectionMap a) where 
  join (SelectionMap k1 x) (SelectionMap k2 y) = SelectionMap (k1 <> k2) <$> safeJoin x y

instance Empty (SelectionMap a) where 
  empty = SelectionMap [] HM.empty

instance Singleton (SelectionMap a) a where
  singleton name = SelectionMap [name] . HM.singleton name 

instance Selectable (SelectionMap a) a where 
  selectOr fb f key SelectionMap { mapEntries } = maybe fb f (HM.lookup key mapEntries)

instance Listable (SelectionMap a) a where
  fromAssoc = safeFromList
  toAssoc SelectionMap {  mapKeys, mapEntries } = map takeValue mapKeys
    where 
      takeValue key = (key, fromMaybe (error "TODO:error") (key `HM.lookup` mapEntries ))

safeFromList :: (Failure GQLErrors m, Applicative m, NameCollision a) => [Named a] -> m (SelectionMap a)
safeFromList values = SelectionMap (map fst values) <$> safeUnionWith HM.empty values 

unsafeFromValues :: KeyOf a => [a] -> SelectionMap a
unsafeFromValues x = SelectionMap (map keyOf x) $ HM.fromList $ map toPair x

safeJoin :: (Failure GQLErrors m, Applicative m, NameCollision a) => HashMap Name a -> HashMap Name a -> m (HashMap Name a)
safeJoin hm newls = safeUnionWith hm (HM.toList newls)

safeUnionWith :: (Failure GQLErrors m, Applicative m, NameCollision a) => HashMap Name a -> [Named a] -> m (HashMap Name a)
safeUnionWith hm names = case insertNoDups (hm,[]) names of 
  (res,dupps) | null dupps -> pure res
              | otherwise -> failure $ map (uncurry nameCollision) dupps

type NoDupHashMap a = (HashMap Name a,[Named a])

insertNoDups :: NoDupHashMap a -> [Named a] -> NoDupHashMap a
insertNoDups collected [] = collected
insertNoDups (coll,errors) (pair@(name,value):xs)
  | isJust (name `HM.lookup` coll) = insertNoDups (coll,errors <> [pair]) xs
  | otherwise = insertNoDups (HM.insert name value coll,errors) xs
