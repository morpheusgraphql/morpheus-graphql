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

data Version = Version Int Int [Int]
  deriving
    ( Generic,
      Eq
    )

getNumber :: [Int] -> Int
getNumber (n : _) = n
getNumber [] = 0

nextVersion :: Bool -> Version -> Version
nextVersion isBreaking (Version major minor revision)
  | isBreaking = Version major (minor + 1) [0]
  | otherwise = Version major minor [getNumber revision + 1]

dropPatch :: Version -> Version
dropPatch (Version ma mi _) = Version ma mi [0]

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
fromSeries [ma] = pure $ Version ma 0 []
fromSeries (ma : (mi : xs)) = pure $ Version ma mi xs
fromSeries [] = fail "invalid version: version should have at least one number !"

instance ToString Version where
  toString (Version maj mi ns) = intercalate "." $ map show ([maj, mi] <> ns)

instance Ord Version where
  compare (Version maj1 min1 v1) (Version maj2 min2 v2) = compareSeries ([maj1, min1] <> v1) ([maj2, min2] <> v2)

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
