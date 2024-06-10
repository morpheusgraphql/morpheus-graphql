{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Feature.NamedResolvers.DB where

import Data.Morpheus.Server.Types (ID (..))
import Relude

allDeities :: [ID]
allDeities = ["zeus", "morpheus"]

allRealms :: [ID]
allRealms = ["olympus", "dreams"]

allEntities :: [ID]
allEntities = ["zeus", "morpheus", "olympus", "dreams"]

getPowers :: (Monad m) => ID -> m [ID]
getPowers "zeus" = pure ["tb"]
getPowers "morpheus" = pure ["sp"]
getPowers _ = pure []

getDeityName :: (Monad m) => ID -> m Text
getDeityName "zeus" = pure "Zeus"
getDeityName "morpheus" = pure "Morpheus"
getDeityName _ = pure ""

getRealmName :: (Monad m) => ID -> m Text
getRealmName "olympus" = pure "Mount Olympus"
getRealmName "dreams" = pure "Fictional world of dreams"
getRealmName _ = pure ""

getOwner :: (Monad m) => ID -> m ID
getOwner "olympus" = pure "zeus"
getOwner "dreams" = pure "morpheus"
getOwner _ = pure ""

getPlace :: (Monad m) => ID -> m ID
getPlace "zeus" = pure "olympus"
getPlace "morpheus" = pure "dreams"
getPlace x = pure x

getDocsById :: (Monad m) => ID -> m (Maybe Text)
getDocsById "morpheus" = pure $ Just "the god of dreams"
getDocsById "zeus" = pure $ Just "the king of the gods"
getDocsById "olympus" = pure $ Just "Mountain"
getDocsById "dreams" = pure $ Just "Dreams"
getDocsById _ = pure Nothing
