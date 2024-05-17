{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Version
  ( Version (..),
    parseVersion,
    VersionBounds (..),
    Deps,
    parseDep,
    printBoundParts,
    printBounds,
    unpackDeps,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    Value (..),
  )
import Data.Char (isSeparator)
import Data.Map (fromList, toList)
import Data.Text
  ( break,
    breakOn,
    drop,
    null,
    pack,
    split,
    strip,
    unpack,
  )
import GHC.Show (Show (show))
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

data Version
  = Version [Int]
  | LatestVersion
  deriving
    ( Generic,
      Eq
    )

instance ToString Version where
  toString (Version ns) = intercalate "." $ map show ns
  toString LatestVersion = "latest"

instance Show Version where
  show = toString

instance ToText Version where
  toText = pack . toString

instance FromJSON Version where
  parseJSON (String s) = parseVersion s
  parseJSON (Number n) = parseVersion $ pack $ show n
  parseJSON v = fail $ "version should be either true or string" <> (show v)

instance ToJSON Version where
  toJSON = String . pack . show

instance Ord Version where
  compare LatestVersion LatestVersion = EQ
  compare LatestVersion (Version _) = GT
  compare (Version _) LatestVersion = LT
  compare (Version v1) (Version v2) = compareSeries v1 v2
    where
      compareSeries [] _ = EQ
      compareSeries _ [] = EQ
      compareSeries (x : xs) (y : ys)
        | x == y = compareSeries xs ys
        | otherwise = compare x y

parseVersion :: (MonadFail m) => Text -> m Version
parseVersion "latest" = pure LatestVersion
parseVersion s =
  maybe
    (fail "invalid version")
    (pure . Version)
    $ traverse (readMaybe . unpack)
    $ (split (== '.') s)

data VersionBounds
  = VersionBounds Version (Maybe Version)
  | NoBounds
  deriving
    ( Generic,
      Show,
      Eq,
      Ord
    )

trim :: (Text, Text) -> (Text, Text)
trim = bimap strip strip

breakOnSPace :: Text -> (Text, Text)
breakOnSPace = trim . break isSeparator

parseVersionTuple :: (MonadFail m) => (Text, Text) -> m VersionBounds
parseVersionTuple (mi, ma) = do
  ((,) <$> parseMin (breakOnSPace mi) <*> parseMax (breakOnSPace ma))
    >>= uncurry parseBoundsFrom

breakAtAnd :: Text -> (Text, Text)
breakAtAnd = trim . second (drop 2) . (breakOn "&&")

parseDep :: (MonadFail m) => Text -> m (Text, VersionBounds)
parseDep = (\(name, bounds) -> (name,) <$> parseVersionBounds bounds) . breakOnSPace

parseVersionBounds :: (MonadFail m) => Text -> m VersionBounds
parseVersionBounds bounds
  | null bounds = pure NoBounds
  | otherwise = parseVersionTuple (breakAtAnd bounds)

parseMax :: (MonadFail m) => (Text, Text) -> m (Maybe Text)
parseMax (o, value) | isUpperConstraint o = pure (Just value)
parseMax _ = pure Nothing

parseMin :: (MonadFail m) => (Text, Text) -> m Text
parseMin (o, value) | isLowerConstraint o = pure value
parseMin (o, v) = fail ("invalid" <> show (o, v))

isLowerConstraint :: Text -> Bool
isLowerConstraint = (`elem` [">", ">="])

isUpperConstraint :: Text -> Bool
isUpperConstraint = (`elem` ["<", "<="])

parseBoundsFrom :: (MonadFail m) => Text -> Maybe Text -> m VersionBounds
parseBoundsFrom minV maxV = VersionBounds <$> (parseVersion minV) <*> (traverse parseVersion maxV)

printBoundParts :: VersionBounds -> [Text]
printBoundParts NoBounds = []
printBoundParts (VersionBounds mi ma) = [">=", toText mi] <> maybe [] (\m -> ["&&", "<", toText m]) ma

printBounds :: VersionBounds -> String
printBounds = intercalate "  " . map toString . printBoundParts

instance FromJSON VersionBounds where
  parseJSON (Bool True) = pure NoBounds
  parseJSON (String s) = parseVersionBounds s
  parseJSON (Number n) = flip VersionBounds Nothing <$> (parseVersion $ pack $ show n)
  parseJSON v = fail $ "version should be either true or string" <> (show v)

instance ToJSON VersionBounds where
  toJSON b
    | NoBounds == b = Bool True
    | otherwise = String $ pack $ printBounds b

type TextDeps = [Text]

newtype Deps = Deps {unpackDeps :: (Map Text VersionBounds)}
  deriving (Show)

parseDependencies :: (MonadFail m) => TextDeps -> m [(Text, VersionBounds)]
parseDependencies = traverse parseDep . sort

instance FromJSON Deps where
  parseJSON v = Deps . fromList <$> (parseJSON v >>= parseDependencies)

instance ToJSON Deps where
  toJSON (Deps m) = toJSON $ map (\(name, b) -> strip $ pack (toString name <> " " <> printBounds b)) (toList m)
