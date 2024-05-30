{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Bounds
  ( Bound (..),
    Restriction (..),
    Bounds (..),
    getBound,
    upperBounds,
    diff,
    printBoundParts,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    Value (..),
  )
import Data.Char (isSeparator)
import Data.Text
  ( null,
    pack,
    unpack,
  )
import qualified Data.Text as T
import GHC.Show (Show (show))
import HConf.Chalk (Color (Yellow), chalk)
import HConf.Parse (Parse (..))
import HConf.Version (Version (..), nextVersion)
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

instance Parse Bounds where
  parse input
    | null str = pure $ Bounds []
    | otherwise = Bounds <$> traverse (parseBound . unpack) (T.splitOn "&&" $ T.filter (not . isSeparator) str)
    where
      str = pack $ toString input

upperBounds :: (MonadFail m) => Version -> m Bounds
upperBounds version = do
  upper <- nextVersion True version
  pure $ Bounds [Bound Min True version, Bound Max False upper]

diff :: Bounds -> Bounds -> String
diff old deps = printBounds old <> chalk Yellow "  ->  " <> printBounds deps

parseOrEquals :: [Char] -> (Bool, [Char])
parseOrEquals ('=' : ver) = (True, ver)
parseOrEquals ver = (False, ver)

instance Parse Bound where
  parse = parseBound . toString
    where
      parseBound (char : str) = do
        res <- parseRestriction char
        let (isStrict, value) = parseOrEquals str
        Bound res isStrict <$> parse value
      parseBound x = fail ("unsorted bound type" <> toString x)

parseRestriction :: (MonadFail f) => Char -> f Restriction
parseRestriction '>' = pure Min -- > 0.7.0
parseRestriction '<' = pure Max -- <  1.0.0
parseRestriction x = fail ("unsorted bound type" <> show x)

printBoundParts :: Bounds -> [Text]
printBoundParts (Bounds xs) = intercalate ["&&"] $ map printBoundPart $ sort xs

getBound :: Restriction -> Bounds -> Maybe Bound
getBound v (Bounds xs) = find (\Bound {..} -> restriction == v) xs

printBoundPart :: Bound -> [Text]
printBoundPart Bound {..} = pack (toString restriction <> if orEquals then "=" else "") : [toText version]

printBounds :: Bounds -> String
printBounds = intercalate "  " . map toString . printBoundParts

instance FromJSON Bounds where
  parseJSON (String s) = parse s
  parseJSON v = fail $ "version should be either true or string" <> show v

instance ToJSON Bounds where
  toJSON = String . pack . printBounds
