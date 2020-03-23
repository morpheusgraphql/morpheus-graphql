{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}

module Data.Morpheus.Types.Internal.Operation
    ( Empty(..)        
    , Selectable(..)
    , Singleton(..)
    , Listable(..)
    , FieldMap(..)
    , Join(..)
    , Failure(..)
    )
    where 

import           Data.Text                              ( Text )
import           Instances.TH.Lift                      ( )
import           Data.HashMap.Lazy                      ( HashMap )
import qualified Data.HashMap.Lazy                   as HM 
import           Data.Morpheus.Types.Internal.AST.Base  ( Name
                                                        , Named
                                                        , GQLErrors
                                                        )
import           Text.Megaparsec.Internal  (ParsecT)
import           Text.Megaparsec.Stream    (Stream)

class Empty a where 
  empty :: a

instance Empty (HashMap k v) where
  empty = HM.empty

class Selectable c a where 
  selectOr :: d -> (a -> d) -> Name -> c -> d

instance Selectable [(Name, a)] a where 
  selectOr fb f key lib = maybe fb f (lookup key lib)

instance Selectable (HashMap Text a) a where 
  selectOr fb f key lib = maybe fb f (HM.lookup key lib)

class Singleton c a where
  singleton  :: Name -> a -> c

class Listable c a where
  fromList   :: (Monad m, Failure GQLErrors m) => [Named a] ->  m c
  toList     ::  c  -> [Named a]

class FieldMap fields field where
  toFields :: fields -> [field] 
  fromFields :: (Monad m, Failure GQLErrors m) => [field] ->  m fields

class Join a where 
  join :: Monad m => a -> a -> m a

class Applicative f => Failure error (f :: * -> *) where
  failure :: error -> f v

instance Failure error (Either error) where
  failure = Left

instance (Stream b) => Failure GQLErrors (ParsecT a b c) where