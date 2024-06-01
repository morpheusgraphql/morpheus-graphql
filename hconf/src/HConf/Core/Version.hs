{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Core.Version
  ( nextVersion,
    dropPatch,
    checkVersion,
    Version,
    fetchVersions,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    Value (..),
  )
import Data.List.NonEmpty (toList)
import Data.Map (lookup)
import Data.Text
  ( pack,
    split,
    unpack,
  )
import GHC.Show (Show (show))
import HConf.Utils.Class (Parse (..))
import HConf.Utils.Core (checkElem)
import HConf.Utils.Http (hackage)
import Relude hiding
  ( Undefined,
    break,
    drop,
    fromList,
    isPrefixOf,
    length,
    null,
    show,
    toList,
  )

data Version = Version Int [Int]
  deriving
    ( Generic,
      Eq
    )

toTriple :: Version -> (Int, Int, Int)
toTriple (Version major (minor : revision : _)) = (major, minor, revision)
toTriple (Version major [minor]) = (major, minor, 0)
toTriple (Version major []) = (major, 0, 0)

nextVersion' :: Bool -> (Int, Int, Int) -> Version
nextVersion' isBreaking (major, minor, revision)
  | isBreaking = Version major [minor + 1, 0]
  | otherwise = Version major [minor, revision + 1]

nextVersion :: Bool -> Version -> Version
nextVersion isBreaking = nextVersion' isBreaking . toTriple

dropPatch :: Version -> Version
dropPatch (Version maj xs) = Version maj (take 1 xs <> [0])

compareSeries :: (Ord a) => [a] -> [a] -> Ordering
compareSeries [] _ = EQ
compareSeries _ [] = EQ
compareSeries (x : xs) (y : ys)
  | x == y = compareSeries xs ys
  | otherwise = compare x y

instance Parse Version where
  parse = parseText . pack
  parseText s = toMonad fromSeries (parseSeries s)
    where
      toMonad = maybe (fail $ "invalid version: '" <> toString s <> "'!")

parseSeries :: Text -> Maybe [Int]
parseSeries = traverse (readMaybe . unpack) . split (== '.')

fromSeries :: (MonadFail m) => [Int] -> m Version
fromSeries (x : xs) = pure $ Version x xs
fromSeries [] = fail "invalid version: version should have at least one number !"

instance ToString Version where
  toString (Version n ns) = intercalate "." $ map show (n : ns)

instance Ord Version where
  compare (Version n1 v1) (Version n2 v2) = compareSeries (n1 : v1) (n2 : v2)

instance Show Version where
  show = toString

instance ToText Version where
  toText = pack . toString

instance FromJSON Version where
  parseJSON (String s) = parseText s
  parseJSON (Number n) = parse (show n)
  parseJSON v = fail $ "version should be either true or string" <> show v

instance ToJSON Version where
  toJSON = String . toText

fetchVersionResponse :: (MonadIO m, MonadFail m) => String -> m (Either String (Map Text (NonEmpty Version)))
fetchVersionResponse name = hackage ["package", name, "preferred"]

lookupVersions :: (MonadFail m) => Either String (Map Text (NonEmpty Version)) -> m (NonEmpty Version)
lookupVersions (Right x) = maybe (fail "field normal-version not found") pure (lookup "normal-version" x)
lookupVersions (Left x) = fail x

fetchVersions :: (MonadFail m, MonadIO m) => String -> m (NonEmpty Version)
fetchVersions name = fetchVersionResponse name >>= lookupVersions

checkVersion :: (MonadFail m, MonadIO m) => (String, Version) -> m ()
checkVersion (name, version) = fetchVersions name >>= checkElem "version" name version . toList
