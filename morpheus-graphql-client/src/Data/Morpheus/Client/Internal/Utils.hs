{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.Utils
  ( removeDuplicates,
    isEnum,
    getSource,
    handleResult,
    getFile,
  )
where

import Control.Monad.Except (MonadError (catchError))
import qualified Data.ByteString.Lazy.Char8 as L
import Data.FileEmbed (makeRelativeToProject)
import Data.List (isSuffixOf)
import Data.Morpheus.Client.Internal.Types
  ( ClientConstructorDefinition (cFields),
    SchemaSource (..),
  )
import Data.Morpheus.Error (gqlWarnings, renderGQLErrors)
import Data.Morpheus.Internal.Ext (GQLResult, Result (..))
import qualified Data.Text.IO as TIO
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (qAddDependentFile)
import Relude

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = fst . splitDuplicates

splitDuplicates :: Eq a => [a] -> ([a], [a])
splitDuplicates = collectElems ([], [])
  where
    collectElems :: Eq a => ([a], [a]) -> [a] -> ([a], [a])
    collectElems collected [] = collected
    collectElems (collected, errors) (x : xs)
      | x `elem` collected = collectElems (collected, errors <> [x]) xs
      | otherwise = collectElems (collected <> [x], errors) xs

isEnum :: [ClientConstructorDefinition] -> Bool
isEnum = all (null . cFields)

getSource :: FilePath -> Q SchemaSource
getSource p
  | ".json" `isSuffixOf` p = JSON <$> readWith L.readFile p
  | ".gql" `isSuffixOf` p || ".graphql" `isSuffixOf` p = GQL <$> readWith L.readFile p
  | otherwise = fail "Unsupported file format! The input should have one of the following extensions: json, gql, graphql"

getFile :: FilePath -> Q Text
getFile = readWith TIO.readFile

readWith :: (FilePath -> IO a) -> FilePath -> Q a
readWith f path = do
  p <- makeRelativeToProject path
  qAddDependentFile p
  file <- runIO (catchError ((fmap Right . f) p) (pure . Left . show))
  case file of
    Left x -> fail x
    Right x -> pure x

handleResult :: GQLResult t -> (t -> Q a) -> Q a
handleResult x f = case x of
  Failure errors -> fail (renderGQLErrors errors)
  Success
    { result,
      warnings
    } -> gqlWarnings warnings >> f result
