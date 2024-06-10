{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleDeityRepoHandler where

import Control.Monad.Freer (Eff, interpretM, runM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find)
import DeityRepo (DeityRepo (..), Error (..))
import Types (Deity (Deity))

deityIORef :: IO (IORef [Deity])
deityIORef = newIORef []

type DeityIORef = IORef [Deity]

exampleDeityRepoHandler :: DeityIORef -> Eff '[DeityRepo, IO] a -> IO a
exampleDeityRepoHandler dbRef =
  runM . interpretM handle
  where
    handle :: DeityRepo v -> IO v
    handle (GetDeityByName name) = do
      (deities :: [Deity]) <- readIORef dbRef
      let result = find (\(Deity name' _) -> name == name') deities
      pure $ toEither (DeityDoesNotExist name) result
    handle (CreateDeity diety) = do
      (deities :: [Deity]) <- readIORef dbRef
      writeIORef dbRef $ addOrReplace diety deities
      pure (Right diety)

addOrReplace :: (Eq a) => a -> [a] -> [a]
addOrReplace a as = a : filter (/= a) as

toEither :: b -> Maybe a -> Either b a
toEither b Nothing = Left b
toEither _ (Just a) = Right a
