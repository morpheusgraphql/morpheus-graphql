{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Morpheus.Types.Internal.AST.OrderedMap
    ( OrderedMap(..)
    )
where 

import           Data.Maybe                             (isJust)
import           Language.Haskell.TH.Syntax             ( Lift(..) )
import           Data.Morpheus.Types.Internal.AST.Base  ( Join(..)
                                                        , Empty(..)
                                                        , Singleton(..)
                                                        , Selectable(..)
                                                        , Listable(..)
                                                        , Name
                                                        , Named(..)
                                                        )
import           Data.HashMap.Lazy                      ( HashMap )
import qualified Data.HashMap.Lazy                      as HM 

-- OrderedMap 
newtype OrderedMap a = OrderedMap { mapValues :: HashMap Name a}
  -- = OrderedMap { mapValues :: [(Name,a)]}
  deriving (Show, Functor, Foldable)

instance Lift a => Lift (OrderedMap a) where
  --lift (OrderedMapError err) = [| OrderedMapError err |]
  lift (OrderedMap x) = [| OrderedMap (HM.fromList ls) |]
    where ls = HM.toList x

instance Traversable OrderedMap where
  traverse f (OrderedMap values) = OrderedMap <$> traverse f values

instance Join (OrderedMap a) where 
  join (OrderedMap x) (OrderedMap y) = fromHashMaps x y

instance Empty (OrderedMap a) where 
  empty = OrderedMap HM.empty

instance Singleton (OrderedMap a) a where
  singleton name = OrderedMap . HM.singleton name 

instance Selectable (OrderedMap a) a where 
  selectOr fb f key OrderedMap { mapValues } = maybe fb f (HM.lookup key mapValues)

instance Listable (OrderedMap a) a where
  fromList = uniqNames
  toList OrderedMap {  mapValues } = map bla $ HM.toList mapValues
    where 
      bla (x,y) = Named x y
    --maybe (error "TODO:error") id $ traverse (`HM.lookup` mapValues) mapNames

uniqNames :: Applicative m => [Named value] -> m (OrderedMap value)
uniqNames values 
  | null dupNames = pure $ OrderedMap $ HM.fromList $ map fromNamed noDups
  -- | otherwise = OrderedMapError dupNames
 where (noDups,dupNames,_) = splitDupElem values

fromHashMaps :: Applicative m => HashMap Name a -> HashMap Name a -> m (OrderedMap a)
fromHashMaps x y = case joinHashmaps x y of 
  (hm,[]) -> pure (OrderedMap hm)
  -- (_,errors) -> OrderedMapError errors


joinHashmaps :: HashMap Name a -> HashMap Name a -> (HashMap Name a,[Name])
joinHashmaps lib newls = collectElems (lib,[]) (HM.toList newls)
 where  
  collectElems :: (HashMap Name a,[Name]) -> [(Name, a)] -> (HashMap Name a,[Name])
  collectElems collected [] = collected
  collectElems (coll,errors) ((name,value):xs)
        | isJust (name `HM.lookup` coll) = collectElems (coll, errors <> [name]) xs
        | otherwise = collectElems (HM.insert name value coll,errors) xs

fromNamed :: Named a -> (Name,a)
fromNamed Named { name , unName} = (name,unName)

splitDupElem :: [Named a] -> ([Named a],[Name],[Named a])
splitDupElem = collectElems ([],[],[])
  where
    collectElems :: ([Named a],[Name],[Named a]) -> [Named a] -> ([Named a],[Name],[Named a])
    collectElems collected [] = collected
    collectElems (values,names,errors) (x:xs)
        | name x `elem` names = collectElems (values,names <> [name x],errors <> [x]) xs
        | otherwise = collectElems (values <> [x],names,errors) xs
