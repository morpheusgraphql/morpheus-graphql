{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Core.Dependencies
  ( Dependencies,
    Dependency,
    getBounds,
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
import Data.Text (break, pack, strip, unpack)
import HConf.Core.Bounds (Bounds, printBoundParts)
import HConf.Utils.Class (Parse (..))
import HConf.Utils.Core (Name)
import HConf.Utils.Format (formatTable)
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

data Dependency = Dependency Name Bounds

instance Parse Dependency where
  parse = parseText . pack
  parseText =
    (\(name, txt) -> Dependency name <$> parseText txt)
      . bimap strip strip
      . break isSeparator

newtype Dependencies = Dependencies {unpackDeps :: Map Name Bounds}
  deriving (Show)

getBounds :: (MonadFail m) => Text -> Dependencies -> m Bounds
getBounds name = maybe (fail $ "Unknown package: " <> unpack name) pure . M.lookup name . unpackDeps

traverseDeps :: (Applicative f) => (Text -> Bounds -> f Bounds) -> Dependencies -> f Dependencies
traverseDeps f (Dependencies xs) = Dependencies <$> traverseWithKey f xs

initDependencies :: [Dependency] -> Dependencies
initDependencies = Dependencies . fromList . map toDuple
  where
    toDuple (Dependency a b) = (a, b)

instance FromJSON Dependencies where
  parseJSON v = initDependencies <$> (parseJSON v >>= traverse parseText . sort)

instance ToJSON Dependencies where
  toJSON (Dependencies m) = toJSON $ formatTable $ map (\(name, b) -> name : printBoundParts b) (toList m)
