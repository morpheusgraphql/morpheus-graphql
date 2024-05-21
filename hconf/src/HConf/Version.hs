{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Version
  ( Version (..),
    VersionBounds (..),
    Deps,
    parseVersion,
    diff,
    getDep,
    traverseDeps,
    Parse (..),
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
    breakOn,
    drop,
    null,
    pack,
    split,
    strip,
    unpack,
  )
import GHC.Show (Show (show))
import HConf.Chalk (Color (Yellow), chalk)
import HConf.Format (formatTable)
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

class Parse a where
  parse :: (ToString t, MonadFail m) => t -> m a

data Version
  = Version [Int]
  | LatestVersion
  deriving
    ( Generic,
      Eq
    )

instance Parse Version where
  parse = parseVersion . pack . toString

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
  parseJSON v = fail $ "version should be either true or string" <> show v

instance ToJSON Version where
  toJSON = String . pack . show

data BoundType
  = Greater
  | Lower
  | GreaterOrEq
  | LowerOrEq
  deriving (Show, Eq, Ord)

parseBound :: (MonadFail f) => Text -> f BoundType
parseBound "<" = pure Greater
parseBound "<=" = pure GreaterOrEq
parseBound ">" = pure Lower
parseBound ">=" = pure LowerOrEq
parseBound x = fail ("unsorted bound type" <> toString x)

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
    $ split (== '.') s

data VersionBounds
  = VersionBounds Version (Maybe Version)
  | NoBounds
  deriving
    ( Generic,
      Show,
      Eq,
      Ord
    )

diff :: VersionBounds -> VersionBounds -> String
diff old deps = printBounds old <> chalk Yellow "  ->  " <> printBounds deps

trim :: (Text, Text) -> (Text, Text)
trim = bimap strip strip

breakOnSPace :: Text -> (Text, Text)
breakOnSPace = trim . break isSeparator

parseBoundTuple :: (MonadFail m) => Text -> m (BoundType, Text)
parseBoundTuple = (\(x, y) -> fmap (,y) (parseBound x)) . breakOnSPace

parseVersionTuple :: (MonadFail m) => (Text, Text) -> m VersionBounds
parseVersionTuple (mi, ma) = do
  ((,) <$> (parseBoundTuple mi >>= parseMin) <*> (parseBoundTuple ma >>= parseMax))
    >>= uncurry parseBoundsFrom

breakAtAnd :: Text -> (Text, Text)
breakAtAnd = trim . second (drop 2) . breakOn "&&"

parseDep :: (MonadFail m) => Text -> m (Text, VersionBounds)
parseDep = (\(name, bounds) -> (name,) <$> parseVersionBounds bounds) . breakOnSPace

parseVersionBounds :: (MonadFail m) => Text -> m VersionBounds
parseVersionBounds bounds
  | null bounds = pure NoBounds
  | otherwise = parseVersionTuple (breakAtAnd bounds)

parseMax :: (MonadFail m) => (BoundType, Text) -> m (Maybe Text)
parseMax (o, value) | isUpperConstraint o = pure (Just value)
parseMax _ = pure Nothing

parseMin :: (MonadFail m) => (BoundType, Text) -> m Text
parseMin (o, value) | isLowerConstraint o = pure value
parseMin (o, v) = fail ("invalid" <> show (o, v))

isLowerConstraint :: BoundType -> Bool
isLowerConstraint = (`elem` [Greater, GreaterOrEq])

isUpperConstraint :: BoundType -> Bool
isUpperConstraint = (`elem` [Lower, LowerOrEq])

parseBoundsFrom :: (MonadFail m) => Text -> Maybe Text -> m VersionBounds
parseBoundsFrom minV maxV = VersionBounds <$> parseVersion minV <*> traverse parseVersion maxV

printBoundParts :: VersionBounds -> [Text]
printBoundParts NoBounds = []
printBoundParts (VersionBounds mi ma) = [">=", toText mi] <> maybe [] (\m -> ["&&", "<", toText m]) ma

printBounds :: VersionBounds -> String
printBounds = intercalate "  " . map toString . printBoundParts

instance FromJSON VersionBounds where
  parseJSON (Bool True) = pure NoBounds
  parseJSON (String s) = parseVersionBounds s
  parseJSON (Number n) = flip VersionBounds Nothing <$> parseVersion (pack $ show n)
  parseJSON v = fail $ "version should be either true or string" <> show v

instance ToJSON VersionBounds where
  toJSON b
    | NoBounds == b = Bool True
    | otherwise = String $ pack $ printBounds b

type TextDeps = [Text]

newtype Deps = Deps {unpackDeps :: Map Text VersionBounds}
  deriving (Show)

traverseDeps :: (Applicative f) => (Text -> VersionBounds -> f VersionBounds) -> Deps -> f Deps
traverseDeps f (Deps xs) = Deps <$> traverseWithKey f xs

getDep :: (MonadFail m) => Text -> Deps -> m VersionBounds
getDep name = maybe (fail $ "Unknown package: " <> unpack name) pure . M.lookup name . unpackDeps

parseDependencies :: (MonadFail m) => TextDeps -> m [(Text, VersionBounds)]
parseDependencies = traverse parseDep . sort

instance FromJSON Deps where
  parseJSON v = Deps . fromList <$> (parseJSON v >>= parseDependencies)

instance ToJSON Deps where
  toJSON (Deps m) = toJSON $ formatTable $ map (\(name, b) -> name : printBoundParts b) (toList m)
