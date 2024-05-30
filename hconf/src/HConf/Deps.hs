{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Deps
  ( Deps,
    getBounds,
    getBound,
    traverseDeps,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
  )
import Data.Char (isSeparator)
import Data.Map (fromList, toList)
import qualified Data.Map as M
import Data.Map.Strict (traverseWithKey)
import Data.Text
  ( break,
    strip,
    unpack,
  )
import HConf.Bounds (Bound (..), Bounds (Bounds), Restriction, printBoundParts)
import HConf.Format (formatTable)
import HConf.Version (Parse (..))
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

trim :: (Text, Text) -> (Text, Text)
trim = bimap strip strip

breakOnSPace :: Text -> (Text, Text)
breakOnSPace = trim . break isSeparator

getBound :: Restriction -> Bounds -> Maybe Bound
getBound v (Bounds xs) = find (\Bound {..} -> restriction == v) xs

newtype Deps = Deps {unpackDeps :: Map Text Bounds}
  deriving (Show)

getBounds :: (MonadFail m) => Text -> Deps -> m Bounds
getBounds name = maybe (fail $ "Unknown package: " <> unpack name) pure . M.lookup name . unpackDeps

traverseDeps :: (Applicative f) => (Text -> Bounds -> f Bounds) -> Deps -> f Deps
traverseDeps f (Deps xs) = Deps <$> traverseWithKey f xs

parseDep :: (MonadFail m) => (Text, Text) -> m (Text, Bounds)
parseDep (name, bounds) = (name,) <$> parse bounds

instance FromJSON Deps where
  parseJSON v = Deps . fromList <$> (parseJSON v >>= traverse (parseDep . breakOnSPace) . sort)

instance ToJSON Deps where
  toJSON (Deps m) = toJSON $ formatTable $ map (\(name, b) -> name : printBoundParts b) (toList m)
