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
    aesonYAMLOptions,
    checkElem,
    notElemError,
  )
where

import Data.Aeson
  ( Options (..),
    defaultOptions,
  )
import Data.Char (isUpper, toLower)
import Data.List (elemIndex, intercalate)
import Data.Text (toTitle)
import Relude hiding (Undefined, intercalate)

aesonYAMLOptions :: Options
aesonYAMLOptions = defaultOptions {fieldLabelModifier = toKebabCase}

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
      "ghc",
      "resolver",
      "packages",
      "groups",
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

compareFieldNames :: (Text, Text) -> Ordering
compareFieldNames t = case mapTuple getIndex t of
  (Nothing, Nothing) -> uncurry compare t
  (Nothing, _) -> GT
  (_, Nothing) -> LT
  (i1, i2) -> compare i1 i2

compareFieldsTuple :: (Text, Text) -> Ordering
compareFieldsTuple = compareFieldNames . mapTuple toTitle

compareFields :: Text -> Text -> Ordering
compareFields = curry compareFieldsTuple

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

notElemError :: (MonadFail m, Eq t, ToString t) => String -> String -> [t] -> m a
notElemError name listName xs =
  fail
    ( "no matching " <> name <> " for '"
        <> listName
        <> "'! try one of: "
        <> intercalate ", " (map toString xs)
    )

checkElem :: (MonadFail m, Eq t, ToString t) => String -> String -> t -> [t] -> m ()
checkElem name listName x xs =
  if x `elem` xs
    then pure ()
    else notElemError name listName xs