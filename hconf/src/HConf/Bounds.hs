{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Bounds
  ( Bound (..),
    Restriction (..),
    Bounds (..),
    genVersionBounds,
    diff,
    printBoundParts,
    updateUpperBound,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    Value (..),
  )
import Data.Char (isSeparator)
import Data.List (maximum)
import Data.Text
  ( null,
    pack,
  )
import qualified Data.Text as T
import GHC.Show (Show (show))
import HConf.Chalk (Color (Yellow), chalk)
import HConf.Http (fetchVersions)
import HConf.Log (Log, field)
import HConf.Parse (Parse (..))
import HConf.Utils (Name)
import HConf.Version (Version (..), dropPatch, nextVersion)
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

parseRestriction :: (MonadFail f) => Char -> f Restriction
parseRestriction '>' = pure Min -- > 0.7.0
parseRestriction '<' = pure Max -- <  1.0.0
parseRestriction x = fail ("unsorted bound type" <> show x)

instance ToString Restriction where
  toString Min = ">" -- >  0.7.0
  toString Max = "<" -- <  1.0.0

instance ToText Restriction where
  toText = pack . toString

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

parseOrEquals :: [Char] -> (Bool, [Char])
parseOrEquals ('=' : ver) = (True, ver)
parseOrEquals ver = (False, ver)

printBoundPart :: Bound -> [Text]
printBoundPart Bound {..} = pack (toString restriction <> if orEquals then "=" else "") : [toText version]

instance Parse Bound where
  parseText = parse . T.unpack
  parse (char : str) = do
    res <- parseRestriction char
    let (isStrict, value) = parseOrEquals str
    Bound res isStrict <$> parse value
  parse x = fail ("unsorted bound type" <> toString x)

newtype Bounds = Bounds [Bound]
  deriving (Generic, Show, Eq)

instance Parse Bounds where
  parse = parseText . pack
  parseText str
    | null str = pure $ Bounds []
    | otherwise = Bounds <$> traverse parseText (T.splitOn "&&" $ T.filter (not . isSeparator) str)

instance FromJSON Bounds where
  parseJSON (String s) = parseText s
  parseJSON v = fail $ "version should be either true or string" <> show v

instance ToJSON Bounds where
  toJSON = String . pack . printBounds

genVersionBounds :: (MonadFail m) => Version -> m Bounds
genVersionBounds version = do
  upper <- nextVersion True version
  pure $ Bounds [Bound Min True (dropPatch version), Bound Max False upper]

diff :: Bounds -> Bounds -> String
diff old deps = printBounds old <> chalk Yellow "  ->  " <> printBounds deps

printBoundParts :: Bounds -> [Text]
printBoundParts (Bounds xs) = intercalate ["&&"] $ map printBoundPart $ sort xs

getBound :: Restriction -> Bounds -> Maybe Bound
getBound v (Bounds xs) = find (\Bound {..} -> restriction == v) xs

printBounds :: Bounds -> String
printBounds = intercalate "  " . map toString . printBoundParts

getLatestBound :: (MonadFail m, MonadIO m) => Name -> m Bound
getLatestBound = fmap (Bound Max True . head) . fetchVersions . T.unpack

updateUpperBound :: (MonadFail m, MonadIO m, Log m) => Text -> Bounds -> m Bounds
updateUpperBound name bounds = do
  latest <- getLatestBound name
  let ma = getBound Max bounds
  let mi = maybeToList (getBound Min bounds)
  let newVersion = maximum (latest : maybeToList ma)
  if ma == Just newVersion then pure () else field (T.unpack name) (show newVersion)
  pure (Bounds (mi <> [newVersion]))