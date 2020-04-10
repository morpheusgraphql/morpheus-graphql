{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}


module Data.Morpheus.Types.Internal.Operation
    ( Empty(..)        
    , Selectable(..)
    , Singleton(..)
    , Listable(..)
    , Merge(..)
    , Failure(..)
    , KeyOf(..)
    , toPair
    , selectBy
    , member
    , keys
    )
    where 

import           Data.Text                              ( Text )
import           Instances.TH.Lift                      ( )
import           Data.HashMap.Lazy                      ( HashMap )
import qualified Data.HashMap.Lazy                   as HM
import           Data.Morpheus.Types.Internal.AST.Base  ( Name
                                                        , Named
                                                        , GQLErrors
                                                        , Ref(..)
                                                        )
import           Text.Megaparsec.Internal               ( ParsecT(..) )
import           Text.Megaparsec.Stream                 ( Stream )

class Empty a where 
  empty :: a

instance Empty (HashMap k v) where
  empty = HM.empty

class Selectable c a | c -> a where 
  selectOr :: d -> (a -> d) -> Name -> c -> d

instance Selectable (HashMap Text a) a where 
  selectOr fb f key lib = maybe fb f (HM.lookup key lib)

selectBy :: (Failure e m, Selectable c a, Monad m) => e -> Name -> c -> m a
selectBy err = selectOr (failure err) pure

member :: forall a c. Selectable c a => Name -> c -> Bool
member = selectOr False toTrue
  where 
    toTrue :: a -> Bool
    toTrue _ = True

class KeyOf a => Singleton c a | c -> a where
  singleton  :: a -> c

class KeyOf a where 
  keyOf :: a -> Name

instance KeyOf Ref where
  keyOf = refName

toPair :: KeyOf a => a -> (Name,a)
toPair x = (keyOf x, x)

class Listable c a | c -> a where
  size :: c -> Int
  size = length . toList 
  fromAssoc   :: (Monad m, Failure GQLErrors m) => [Named a] ->  m c
  toAssoc     ::  c  -> [Named a]
  fromList :: (KeyOf a, Monad m, Failure GQLErrors m) => [a] ->  m c
  -- TODO: fromValues
  toList = map snd . toAssoc 
  fromList = fromAssoc . map toPair  
  -- TODO: toValues    
  toList :: c -> [a] 

keys :: Listable c a  => c -> [Name]
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
