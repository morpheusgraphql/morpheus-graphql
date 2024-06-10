{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DeityRepo where

import Control.Monad.Freer (Eff, Member, send)
import Types (Deity, Name)

data Error = DeityDoesNotExist Name | Unknown deriving (Show)

data DeityRepo r where
  GetDeityByName :: Name -> DeityRepo (Either Error Deity)
  CreateDeity :: Deity -> DeityRepo (Either Error Deity)

-- Interface for use
getDeityByName :: (Member DeityRepo effs) => Name -> Eff effs (Either Error Deity)
getDeityByName name = send $ GetDeityByName name

createDeity :: (Member DeityRepo effs) => Deity -> Eff effs (Either Error Deity)
createDeity deity = send $ CreateDeity deity
