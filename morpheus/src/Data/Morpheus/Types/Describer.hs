{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Types.Describer
  ( (::->)(..)
  ) where

import           GHC.Generics (Generic)

newtype a ::-> b =
  Resolver (a -> IO (Either String b))
  deriving (Generic)
