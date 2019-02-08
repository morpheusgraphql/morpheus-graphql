{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}

module Data.GraphqlHS.Generics.Resolver
    ( Resolver
    , resolve
    )
where

import           GHC.Generics                   ( Generic )
import           Data.GraphqlHS.Types.Types     ( Eval(..) )
import           Data.Data                      ( Data )
import           Data.GraphqlHS.ErrorMessage    ( unknownArgument )

class Resolver p a where
    resolve :: p -> Maybe a -> IO a
