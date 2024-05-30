{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Deps
  ( Deps,
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
import HConf.Bounds (Bound (..), Bounds (Bounds), Restriction)
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

printBoundParts :: Bounds -> [Text]
printBoundParts (Bounds xs) = intercalate ["&&"] $ map printBoundPart $ sort xs

getBound :: Restriction -> Bounds -> Maybe Bound
getBound v (Bounds xs) = find (\Bound {..} -> restriction == v) xs

instance FromJSON Bounds where
  parseJSON (String s) = parseBounds s
  parseJSON v = fail $ "version should be either true or string" <> show v

instance ToJSON Bounds where
  toJSON = String . pack . printBounds

newtype Deps = Deps {unpackDeps :: Map Text Bounds}
  deriving (Show)

getBounds :: (MonadFail m) => Text -> Deps -> m Bounds
getBounds name = maybe (fail $ "Unknown package: " <> unpack name) pure . M.lookup name . unpackDeps

traverseDeps :: (Applicative f) => (Text -> Bounds -> f Bounds) -> Deps -> f Deps
traverseDeps f (Deps xs) = Deps <$> traverseWithKey f xs

parseDep :: (MonadFail m) => (Text, Text) -> m (Text, Bounds)
parseDep (name, bounds) = (name,) <$> parseBounds bounds

instance FromJSON Deps where
  parseJSON v = Deps . fromList <$> (parseJSON v >>= traverse (parseDep . breakOnSPace) . sort)

instance ToJSON Deps where
  toJSON (Deps m) = toJSON $ formatTable $ map (\(name, b) -> name : printBoundParts b) (toList m)
