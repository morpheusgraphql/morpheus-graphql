{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DeityRepo (DeityRepo, getDeityByName) where

import           Control.Monad.Freer       (Eff, Member, send)
import           Control.Monad.Freer.State ()



data Deity = Deity {
  name  :: Name,
  power :: Power
} deriving (Show)

type Name = String
type Power = String
data Error = DeityDoesNotExist Name | Unknown

data DeityRepo r where
  GetDeityByName :: Name -> DeityRepo (Either Error Deity)

-- Interface for use
getDeityByName :: Member DeityRepo effs => Name -> Eff effs (Either Error Deity)
getDeityByName name = send $ GetDeityByName name
