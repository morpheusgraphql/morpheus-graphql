{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleDeityRepoHandler (exampleDeityRepoHandler) where

import           DeityRepo (DeityRepo (..), Error (..))
import           Control.Monad.Freer                          (Eff, reinterpret, runM, interpretM,
                                                               run)
import Control.Monad.Freer.State
import           Data.List                                    (find)
import Types

exampleDeityRepoHandler :: Eff '[DeityRepo, IO] a -> IO a
exampleDeityRepoHandler request = 
  runM . interpretM handle
  where
    handle :: DeityRepo v -> IO v
    handle (GetDeityByName name) = do
      (deities :: [Deity]) <- get
      let result = find (\(Deity name' _) -> name == name') deities
      pure $ toEither (DeityDoesNotExist name) result

    handle (CreateDeity diety) = do
      (deities :: [Deity]) <- get
      put (diety : deities)
      pure (Right ())

toEither :: b -> Maybe a -> Either b a
toEither b Nothing  = Left b
toEither _ (Just a) = Right a
