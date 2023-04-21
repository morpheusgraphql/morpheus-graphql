{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Morpheus.JSONDiff
  ( jsonEQ,
  )
where

import Data.Aeson (encode)
import Data.Aeson.KeyMap (KeyMap, keys, lookup)
import Data.Aeson.Types (Key, ToJSON (..), Value (..))
import Data.ByteString.Lazy.Char8 (unpack)
import GHC.Show (Show (show))
import Relude hiding (ByteString, Show, show)
import Test.Tasty.HUnit (assertFailure)

data JSONDiff
  = DiffNode [(Key, JSONDiff)]
  | DiffLeaf Value Value

instance Show JSONDiff where
  show (DiffNode xs) = intercalate "\n" (map showField xs)
    where
      showField (k, v) = unescape (show k) <> ":\n  " <> indent (show v)
  show (DiffLeaf x y) = "expected: " <> unpack (encode x) <> "\ngot:" <> unpack (encode y) <> ")"

unescape :: String -> String
unescape = concatMap f
  where
    f '\"' = ""
    f x = x : ""

indent :: String -> String
indent = concatMap f
  where
    f '\n' = "\n  "
    f x = x : ""

diff :: Value -> Value -> Maybe JSONDiff
diff (Object beforeFields) (Object afterFields)
  | null changes = Nothing
  | otherwise = Just (DiffNode changes)
  where
    changes = concatMap (\key -> (key,) <$> getFieldPair key) ks
    getFieldPair key = maybeToList $ diff (getField key beforeFields) (getField key afterFields)
    ks = uniq (keys (beforeFields <> afterFields))
diff v1 v2
  | v1 == v2 =
      Nothing
  | otherwise = Just (DiffLeaf v1 v2)

getField :: Key -> KeyMap Value -> Value
getField key = fromMaybe Null . lookup key

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x : xs)
  | isJust (find (== x) xs) = uniq xs
  | otherwise = x : uniq xs

jsonEQ :: ToJSON a => a -> a -> IO ()
jsonEQ expected actual = case diff (toJSON expected) (toJSON actual) of
  Just x -> assertFailure $ indent $ "\n" <> show x <> "\n"
  Nothing -> pure ()
