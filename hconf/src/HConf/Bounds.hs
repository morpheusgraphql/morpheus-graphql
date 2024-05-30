{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Bounds
  ( Bound (..),
    Restriction (..),
    Bounds (..),
    Deps,
    getBounds,
    getBound,
    traverseDeps,
    upperBounds,
    diff,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    Value (..),
  )
import Data.Char (isSeparator)
import Data.Map (fromList, toList)
import qualified Data.Map as M
import Data.Map.Strict (traverseWithKey)
import Data.Text
  ( break,
    null,
    pack,
    strip,
    unpack,
  )
import qualified Data.Text as T
import GHC.Show (Show (show))
import HConf.Chalk (Color (Yellow), chalk)
import HConf.Format (formatTable)
import HConf.Version (Parse (..), Version (..), nextVersion)
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

data Restriction = Min | Max deriving (Show, Eq, Ord)

data Bound = Bound
  { restriction :: Restriction,
    orEquals :: Bool,
    version :: Version
  }
  deriving (Show, Eq)

instance Ord Bound where
  compare a b =
    compare (version a) (version b)
      <> compare (restriction a) (restriction b)
      <> compare (orEquals a) (orEquals b)

instance ToString Restriction where
  toString Min = ">" -- >  0.7.0
  toString Max = "<" -- <  1.0.0

instance ToText Restriction where
  toText = pack . toString

newtype Bounds = Bounds [Bound]
  deriving (Generic, Show, Eq)

upperBounds :: (MonadFail m) => Version -> m Bounds
upperBounds version = do
  upper <- nextVersion True version
  pure $ Bounds [Bound Min True version, Bound Max False upper]

diff :: Bounds -> Bounds -> String
diff old deps = printBounds old <> chalk Yellow "  ->  " <> printBounds deps

trim :: (Text, Text) -> (Text, Text)
trim = bimap strip strip

breakOnSPace :: Text -> (Text, Text)
breakOnSPace = trim . break isSeparator

parseBound :: (MonadFail f) => String -> f Bound
parseBound (h : t) = do
  res <- parseRestriction h
  let (isStrict, value) = parseOrEquals t
  ver <- parse value
  pure $ Bound res isStrict ver
parseBound x = fail ("unsorted bound type" <> toString x)

parseRestriction :: (MonadFail f) => Char -> f Restriction
parseRestriction '>' = pure Min -- > 0.7.0
parseRestriction '<' = pure Max -- <  1.0.0
parseRestriction x = fail ("unsorted bound type" <> show x)

parseOrEquals :: [Char] -> (Bool, [Char])
parseOrEquals ('=' : ver) = (True, ver)
parseOrEquals ver = (False, ver)

parseBounds :: (MonadFail m) => Text -> m Bounds
parseBounds bounds
  | null bounds = pure $ Bounds []
  | otherwise = Bounds <$> traverse (parseBound . unpack) (T.splitOn "&&" $ T.filter (not . isSeparator) bounds)

parseDep :: (MonadFail m) => (Text, Text) -> m (Text, Bounds)
parseDep (name, bounds) = (name,) <$> parseBounds bounds

printBoundParts :: Bounds -> [Text]
printBoundParts (Bounds xs) = intercalate ["&&"] $ map printBoundPart $ sort xs

getBound :: Restriction -> Bounds -> Maybe Bound
getBound v (Bounds xs) = find (\Bound {..} -> restriction == v) xs

printBoundPart :: Bound -> [Text]
printBoundPart Bound {..} = pack (toString restriction <> if orEquals then "=" else "") : [toText version]

printBounds :: Bounds -> String
printBounds = intercalate "  " . map toString . printBoundParts

instance FromJSON Bounds where
  parseJSON (String s) = parseBounds s
  parseJSON v = fail $ "version should be either true or string" <> show v

instance ToJSON Bounds where
  toJSON = String . pack . printBounds

type TextDeps = [Text]

newtype Deps = Deps {unpackDeps :: Map Text Bounds}
  deriving (Show)

traverseDeps :: (Applicative f) => (Text -> Bounds -> f Bounds) -> Deps -> f Deps
traverseDeps f (Deps xs) = Deps <$> traverseWithKey f xs

getBounds :: (MonadFail m) => Text -> Deps -> m Bounds
getBounds name = maybe (fail $ "Unknown package: " <> unpack name) pure . M.lookup name . unpackDeps

parseDependencies :: (MonadFail m) => TextDeps -> m [(Text, Bounds)]
parseDependencies = traverse (parseDep . breakOnSPace) . sort

instance FromJSON Deps where
  parseJSON v = Deps . fromList <$> (parseJSON v >>= parseDependencies)

instance ToJSON Deps where
  toJSON (Deps m) = toJSON $ formatTable $ map (\(name, b) -> name : printBoundParts b) (toList m)
