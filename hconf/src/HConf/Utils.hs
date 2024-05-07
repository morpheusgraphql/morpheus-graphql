{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Utils
  ( compareFields,
    maybeList,
    toKebabCase,
    Name,
    tupled,
  )
where

import Data.Char (isUpper, toLower)
import Data.List (findIndex)
import Data.Text (toTitle)
import HConf.Version (parseVersion)
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
getIndex x = findIndex (== x) fields

compareFieldNames :: Text -> Text -> Ordering
compareFieldNames x y = case (getIndex x, getIndex y) of
  (Nothing, Nothing) -> case (parseVersion x, parseVersion y) of
    (Just v1, Just v2) -> compare v1 v2
    _ -> compare x y
  (Nothing, _) -> GT
  (_, Nothing) -> LT
  (i1, i2) -> compare i1 i2

compareFields :: Text -> Text -> Ordering
compareFields x y = compareFieldNames (toTitle x) (toTitle y)

maybeList :: Maybe [a] -> [a]
maybeList = fromMaybe []

toKebabCase :: String -> String
toKebabCase = concatMap toKebab
  where
    toKebab
      x
        | isUpper x = ['-', (toLower x)]
        | otherwise = [x]

tupled :: (Functor f) => (t -> f a) -> t -> f (t, a)
tupled f p = (p,) <$> f p
