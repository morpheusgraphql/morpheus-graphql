{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DeityRepo (DeityRepo, getDeityByName) where

import           Control.Monad.Freer
import           Control.Monad.Freer.State
import Types

data Error = DeityDoesNotExist Name | Unknown deriving Show

data DeityRepo r where
  GetDeityByName :: Name -> DeityRepo (Either Error Deity)

-- Interface for use
getDeityByName :: Member DeityRepo effs => Name -> Eff effs (Either Error Deity)
getDeityByName name = send $ GetDeityByName name
