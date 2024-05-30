{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Utils.Core
  ( compareFields,
    maybeList,
    toKebabCase,
    Name,
    tupled,
  )
where

import Data.Char (isUpper, toLower)
import Data.List (elemIndex)
import Data.Text (toTitle)
import HConf.Utils.Parse (Parse (..))
import Relude hiding (Undefined, intercalate)

type Name = Text

fields :: [Text]
fields =
  map
    toTitle
    [ "name",
      "version",
      "github",
      "license",
      "author",
      "category",
      "synopsis",
      "maintainer",
      "homepage",
      "copyright",
      "license-file",
      "description",
      "bounds",
      "resolver",
      "packages",
      "builds",
      "extra-source-files",
      "data-files",
      "main",
      "source-dirs",
      "ghc-options",
      "dependencies",
      "library",
      "executables",
      "include",
      "exclude",
      "allow-newer",
      "save-hackage-creds",
      "extra-deps",
      "stackYaml",
      "components",
      "path",
      "component"
    ]

getIndex :: Text -> Maybe Int
getIndex = (`elemIndex` fields)

type TupleRes a = (Text, Text) -> (a, a)

mapTuple :: (Text -> a) -> TupleRes a
mapTuple f = bimap f f

parseAs :: (Parse a) => Proxy a -> TupleRes (Maybe a)
parseAs _ = mapTuple parseText

compareFieldNames :: (Parse a, Ord a) => Proxy a -> (Text, Text) -> Ordering
compareFieldNames proxy t = case mapTuple getIndex t of
  (Nothing, Nothing) -> case parseAs proxy t of
    (Just v1, Just v2) -> compare v1 v2
    _ -> uncurry compare t
  (Nothing, _) -> GT
  (_, Nothing) -> LT
  (i1, i2) -> compare i1 i2

compareFieldsTuple :: (Parse a, Ord a) => Proxy a -> (Text, Text) -> Ordering
compareFieldsTuple proxy = compareFieldNames proxy . mapTuple toTitle

compareFields :: (Parse a, Ord a) => Proxy a -> Text -> Text -> Ordering
compareFields proxy = curry (compareFieldsTuple proxy)

maybeList :: Maybe [a] -> [a]
maybeList = fromMaybe []

toKebabCase :: String -> String
toKebabCase = concatMap toKebab
  where
    toKebab
      x
        | isUpper x = ['-', toLower x]
        | otherwise = [x]

tupled :: (Functor f) => (t -> f a) -> t -> f (t, a)
tupled f p = (p,) <$> f p
