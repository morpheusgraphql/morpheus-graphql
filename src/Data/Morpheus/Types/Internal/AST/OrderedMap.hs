{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Data.Morpheus.Types.Internal.AST.OrderedMap
    ( OrderedMap
    , unsafeFromList
    )
where 

import           Data.HashMap.Lazy                      ( HashMap )
import qualified Data.HashMap.Lazy                      as HM 
import           Data.Semigroup                         ((<>))
import           Data.Maybe                             (isJust,fromMaybe)
import           Language.Haskell.TH.Syntax             ( Lift(..) )

-- MORPHEUS
import           Data.Morpheus.Error.Utils              (duplicateKeyError)
import           Data.Morpheus.Types.Internal.Operation ( Join(..)
                                                        , Empty(..)
                                                        , Singleton(..)
                                                        , Selectable(..)
                                                        , Listable(..)
                                                        , Failure(..)
                                                        )
import           Data.Morpheus.Types.Internal.AST.Base  ( Name
                                                        , Named
                                                        , GQLErrors
                                                        )


-- OrderedMap 
data OrderedMap a = OrderedMap { 
    mapKeys :: [Name], 
    mapEntries :: HashMap Name a 
  } deriving (Show, Functor)

instance Lift a => Lift (OrderedMap a) where
  lift (OrderedMap names x) = [| OrderedMap names (HM.fromList ls) |]
    where ls = HM.toList x

instance Foldable OrderedMap where
  foldMap f OrderedMap { mapEntries } = foldMap f mapEntries

instance Traversable OrderedMap where
  traverse f (OrderedMap names values) = OrderedMap names <$> traverse f values

instance Join (OrderedMap a) where 
  join (OrderedMap k1 x) (OrderedMap k2 y) = OrderedMap (k1 <> k2) <$> safeJoin x y

instance Empty (OrderedMap a) where 
  empty = OrderedMap [] HM.empty

instance Singleton (OrderedMap a) a where
  singleton name = OrderedMap [name] . HM.singleton name 

instance Selectable (OrderedMap a) a where 
  selectOr fb f key OrderedMap { mapEntries } = maybe fb f (HM.lookup key mapEntries)

instance Listable (OrderedMap a) a where
  fromAssoc = safeFromList
  toAssoc OrderedMap {  mapKeys, mapEntries } = map takeValue mapKeys
    where 
      takeValue key = (key, fromMaybe (error "TODO:error") (key `HM.lookup` mapEntries ))

safeFromList :: (Failure GQLErrors m, Applicative m) => [Named a] -> m (OrderedMap a)
safeFromList values = OrderedMap (map fst values) <$> safeUnionWith HM.empty values 

unsafeFromList :: [(Name, a)] -> OrderedMap a
unsafeFromList x = OrderedMap (map fst x) $ HM.fromList x

safeJoin :: (Failure GQLErrors m, Applicative m) => HashMap Name a -> HashMap Name a -> m (HashMap Name a)
safeJoin hm newls = safeUnionWith hm (HM.toList newls)

safeUnionWith :: (Failure GQLErrors m, Applicative m) => HashMap Name a -> [Named a] -> m (HashMap Name a)
safeUnionWith hm names = case insertNoDups (hm,[]) names of 
  (res,dupps) | null dupps -> pure res
              | otherwise -> failure $ map duplicateKeyError dupps

type NoDupHashMap a = (HashMap Name a,[Named a])

insertNoDups :: NoDupHashMap a -> [Named a] -> NoDupHashMap a
insertNoDups collected [] = collected
insertNoDups (coll,errors) (pair@(name,value):xs)
  | isJust (name `HM.lookup` coll) = insertNoDups (coll,pair:errors) xs
  | otherwise = insertNoDups (HM.insert name value coll,errors) xs

