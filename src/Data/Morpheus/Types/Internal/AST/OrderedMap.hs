{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.Morpheus.Types.Internal.AST.OrderedMap
    ( OrderedMap
    , unsafeFromList
    )
where 

import           Data.Semigroup                         ((<>))
import           Data.Maybe                             (isJust,fromMaybe)
import           Language.Haskell.TH.Syntax             ( Lift(..) )
import           Data.Morpheus.Types.Internal.Operation ( Join(..)
                                                        , Empty(..)
                                                        , Singleton(..)
                                                        , Selectable(..)
                                                        , Listable(..)
                                                        , Failure(..)
                                                        , FieldMap(..)
                                                        )
import           Data.Morpheus.Types.Internal.AST.Base  ( Name
                                                        , Named
                                                        , GQLErrors
                                                        , GQLError(..)
                                                        )
import           Data.HashMap.Lazy                      ( HashMap )
import qualified Data.HashMap.Lazy                      as HM 

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
  join (OrderedMap k1 x) (OrderedMap k2 y) = OrderedMap (k1 <> k2) <$> fromHashMaps x y

instance Empty (OrderedMap a) where 
  empty = OrderedMap [] HM.empty

instance Singleton (OrderedMap a) a where
  singleton name = OrderedMap [name] . HM.singleton name 

instance Selectable (OrderedMap a) a where 
  selectOr fb f key OrderedMap { mapEntries } = maybe fb f (HM.lookup key mapEntries)

instance Listable (OrderedMap a) a where
  fromList = uniqNames
  toList OrderedMap {  mapKeys, mapEntries } = map takeValue mapKeys
    where 
      takeValue key = (key, fromMaybe (error "TODO:error") (key `HM.lookup` mapEntries ))

-- TODO: move into errors, better formating
duplicateKeyError :: (Name,a) -> GQLError
duplicateKeyError (name,_) = GQLError { message = "duplicate key \"" <> name <> "\"", locations = []}

fromHashMaps :: (Failure GQLErrors m, Applicative m)=> HashMap Name a -> HashMap Name a -> m (HashMap Name a)
fromHashMaps x y = case joinHashmaps x y of 
  (hm,[]) -> pure hm
  -- (_,errors) -> failure $ [] -- TODO:

uniqNames :: (Failure GQLErrors m, Applicative m) => [Named value] -> m (OrderedMap value)
uniqNames values 
  | null dups = pure $ OrderedMap {
        mapKeys = map fst noDups,
        mapEntries = HM.fromList noDups
      }
  | otherwise = failure $ map duplicateKeyError dups
 where 
    (noDups,dups) = splitDupElem values

unsafeFromList :: [(Name, a)] -> OrderedMap a
unsafeFromList x = OrderedMap (map fst x) $ HM.fromList x

joinHashmaps :: HashMap Name a -> HashMap Name a -> (HashMap Name a,[Name])
joinHashmaps lib newls = collectElems (lib,[]) (HM.toList newls)
 where  
  collectElems :: (HashMap Name a,[Name]) -> [(Name, a)] -> (HashMap Name a,[Name])
  collectElems collected [] = collected
  collectElems (coll,errors) ((name,value):xs)
        | isJust (name `HM.lookup` coll) = collectElems (coll, errors <> [name]) xs
        | otherwise = collectElems (HM.insert name value coll,errors) xs

splitDupElem :: [Named a] -> ([Named a],[Named a])
splitDupElem x = (noDups,dups)
  where
    (noDups,_,dups) = collectElems ([],[],[]) x
    collectElems :: ([Named a],[Name],[Named a]) -> [Named a] -> ([Named a],[Name],[Named a])
    collectElems collected [] = collected
    collectElems (values,names,errors) (x:xs)
        | fst x `elem` names = collectElems (values,names,errors <> [x]) xs
        | otherwise = collectElems (values <> [x],names <> [fst x],errors) xs
