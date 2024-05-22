{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Version
  ( Version (..),
    Bounds (..),
    Deps,
    diff,
    getDep,
    traverseDeps,
    Parse (..),
    nextVersion,
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
  parse :: (ToString t, IsString t, Eq t, MonadFail m) => t -> m a

data Version
  = Version [Int]
  | LatestVersion
  deriving
    ( Generic,
      Eq
    )

nextVersion :: (MonadFail m) => Bool -> Version -> m Version
nextVersion isBreaking (Version [major, minor, revision])
  | isBreaking = pure $ Version [major, minor + 1, 0]
  | otherwise = pure $ Version [major, minor, revision + 1]
nextVersion _ v = fail $ "can't update version " <> show v

instance Parse Version where
  parse "latest" = pure LatestVersion
  parse s =
    maybe
      (fail $ "invalid version: '" <> toString s <> "'!")
      (pure . Version)
      $ traverse (readMaybe . unpack)
      $ split (== '.')
      $ pack
      $ toString s

instance ToString Version where
  toString (Version ns) = intercalate "." $ map show ns
  toString LatestVersion = "latest"

instance Show Version where
  show = toString

instance ToText Version where
  toText = pack . toString

instance FromJSON Version where
  parseJSON (String s) = parse s
  parseJSON (Number n) = parse (show n)
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

data Bounds
  = Bounds Version (Maybe Version)
  | NoBounds
  deriving
    ( Generic,
      Show,
      Eq,
      Ord
    )

diff :: Bounds -> Bounds -> String
diff old deps = printBounds old <> chalk Yellow "  ->  " <> printBounds deps

trim :: (Text, Text) -> (Text, Text)
trim = bimap strip strip

breakOnSPace :: Text -> (Text, Text)
breakOnSPace = trim . break isSeparator

parseBoundTuple :: (MonadFail m) => Text -> (m BoundType, Text)
parseBoundTuple = first parseBound . breakOnSPace

parseVersionTuple :: (MonadFail m) => (Text, Text) -> m Bounds
parseVersionTuple (mi, ma) = do
  ((,) <$> parseMin (parseBoundTuple mi) <*> parseMax (parseBoundTuple ma))
    >>= uncurry parseBoundsFrom

breakAtAnd :: Text -> (Text, Text)
breakAtAnd = trim . second (drop 2) . breakOn "&&"

parseDep :: (MonadFail m) => Text -> m (Text, Bounds)
parseDep = (\(name, bounds) -> (name,) <$> parseVersionBounds bounds) . breakOnSPace

parseVersionBounds :: (MonadFail m) => Text -> m Bounds
parseVersionBounds bounds
  | null bounds = pure NoBounds
  | otherwise = parseVersionTuple (breakAtAnd bounds)

parseMax :: (MonadFail m) => (Either Text BoundType, Text) -> m (Maybe Text)
parseMax (Left {}, v) | null v = pure Nothing
parseMax (Right o, value) | isUpperConstraint o = pure (Just value)
parseMax _ = pure Nothing

parseMin :: (MonadFail m) => (Either Text BoundType, Text) -> m Text
parseMin (Right o, v) | isLowerConstraint o = pure v
parseMin (o, v) = fail ("invalid" <> show (o, v))

isLowerConstraint :: BoundType -> Bool
isLowerConstraint = (`elem` [Lower, LowerOrEq])

isUpperConstraint :: BoundType -> Bool
isUpperConstraint = (`elem` [Greater, GreaterOrEq])

parseBoundsFrom :: (MonadFail m) => Text -> Maybe Text -> m Bounds
parseBoundsFrom minV maxV = Bounds <$> parse minV <*> traverse parse maxV

printBoundParts :: Bounds -> [Text]
printBoundParts NoBounds = []
printBoundParts (Bounds mi ma) = [">=", toText mi] <> maybe [] (\m -> ["&&", "<", toText m]) ma

printBounds :: Bounds -> String
printBounds = intercalate "  " . map toString . printBoundParts

instance FromJSON Bounds where
  parseJSON (Bool True) = pure NoBounds
  parseJSON (String s) = parseVersionBounds s
  parseJSON (Number n) = flip Bounds Nothing <$> parse (pack $ show n)
  parseJSON v = fail $ "version should be either true or string" <> show v

instance ToJSON Bounds where
  toJSON b
    | NoBounds == b = Bool True
    | otherwise = String $ pack $ printBounds b

type TextDeps = [Text]

newtype Deps = Deps {unpackDeps :: Map Text Bounds}
  deriving (Show)

traverseDeps :: (Applicative f) => (Text -> Bounds -> f Bounds) -> Deps -> f Deps
traverseDeps f (Deps xs) = Deps <$> traverseWithKey f xs

getDep :: (MonadFail m) => Text -> Deps -> m Bounds
getDep name = maybe (fail $ "Unknown package: " <> unpack name) pure . M.lookup name . unpackDeps

parseDependencies :: (MonadFail m) => TextDeps -> m [(Text, Bounds)]
parseDependencies = traverse parseDep . sort

instance FromJSON Deps where
  parseJSON v = Deps . fromList <$> (parseJSON v >>= parseDependencies)

instance ToJSON Deps where
  toJSON (Deps m) = toJSON $ formatTable $ map (\(name, b) -> name : printBoundParts b) (toList m)
