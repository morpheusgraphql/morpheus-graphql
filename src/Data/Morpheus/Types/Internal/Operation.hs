{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}

module Data.Morpheus.Types.Internal.Operation
    ( Empty(..)        
    , Selectable(..)
    , Singleton(..)
    , Listable(..)
    , Join(..)
    , Failure(..)
    , KeyOf(..)
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
import           Text.Megaparsec.Internal               ( ParsecT(..) )
import           Text.Megaparsec.Stream                 ( Stream )


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

class KeyOf a where 
  keyOf :: a -> Name

instance KeyOf (Name,a) where
  keyOf = fst

toTuple :: KeyOf a => a -> (Name,a)
toTuple x = (keyOf x, x)

class Listable c a where
  fromAssoc   :: (Monad m, Failure GQLErrors m) => [Named a] ->  m c
  toAssoc     ::  c  -> [Named a]
  fromFields :: (KeyOf a, Monad m, Failure GQLErrors m) => [a] ->  m c
  toFields = map snd . toAssoc 
  fromFields = fromAssoc . map toTuple      
  toFields :: c -> [a] 

class Join a where 
  join :: (Monad m, Failure GQLErrors m) => a -> a -> m a


class Applicative f => Failure error (f :: * -> *) where
  failure :: error -> f v

instance Failure error (Either error) where
  failure = Left

instance (Stream s, Ord e, Failure [a] m) => Failure [a] (ParsecT e s m) where
  failure x = ParsecT $ \_ _ _ _ _ -> failure x