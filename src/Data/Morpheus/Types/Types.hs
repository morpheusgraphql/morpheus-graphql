{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE KindSignatures   #-}

module Data.Morpheus.Types.Types
  ( Undefined(..)
  )
where

import           GHC.Generics                   ( Generic )

data Undefined (m :: * -> *) = Undefined deriving (Show, Generic)
