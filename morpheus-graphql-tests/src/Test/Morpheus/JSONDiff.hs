{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Morpheus.JSONDiff
  ( jsonEQ,
  )
where

import Data.Aeson (ToJSON (..), Value (..), encode)
import Data.ByteString.Lazy.Char8 (unpack)
import GHC.Show (Show (show))
import Relude hiding (ByteString, Show, show)
import Test.Tasty.HUnit (assertFailure)

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap (keys, lookup)
# else
import Data.HashMap.Lazy (keys, lookup)
#endif

data Diff
  = DiffNode [(String, Diff)]
  | DiffLeaf Value Value

instance Show Diff where
  show (DiffNode xs) = intercalate "\n" (map showField xs)
    where
      showField (k, v) = k <> ":\n  " <> indent (show v)
  show (DiffLeaf x y) =
    "should be:"
      <> showLeaf x
      <> "but it is:"
      <> showLeaf y

showLeaf :: (ToJSON a) => a -> [Char]
showLeaf x = " " <> unpack (encode x) <> "\n"

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

diff :: (Value, Value) -> Maybe Diff
diff (Object beforeFields, Object afterFields) = diffNode $ map toPair ks
  where
    ks = uniq (keys (beforeFields <> afterFields))
    toPair key = (unescape (show key), (getField key beforeFields, getField key afterFields))
    getField key = fromMaybe Null . lookup key
diff (Array beforeElems, Array afterElems) = diffNode (zip ks vs)
  where
    ks = map show ([1 ..] :: [Int])
    vs = zipOptional (toList beforeElems) (toList afterElems)
diff (v1, v2)
  | v1 == v2 =
      Nothing
  | otherwise = Just (DiffLeaf v1 v2)

diffNode :: [(String, (Value, Value))] -> Maybe Diff
diffNode values
  | null entries = Nothing
  | otherwise = Just (DiffNode entries)
  where
    entries = mapMaybe (\(key, value) -> (key,) <$> diff value) values

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x : xs)
  | isJust (find (== x) xs) = uniq xs
  | otherwise = x : uniq xs

jsonEQ :: (ToJSON a) => a -> a -> IO ()
jsonEQ expected actual = case diff (toJSON expected, toJSON actual) of
  Just x -> assertFailure $ indent $ "\n" <> show x <> "\n"
  Nothing -> pure ()

zipOptional :: [Value] -> [Value] -> [(Value, Value)]
zipOptional [] [] = []
zipOptional (x : xs) [] = (x, Null) : zipOptional xs []
zipOptional [] (y : ys) = (Null, y) : zipOptional [] ys
zipOptional (x : xs) (y : ys) = (x, y) : zipOptional xs ys
