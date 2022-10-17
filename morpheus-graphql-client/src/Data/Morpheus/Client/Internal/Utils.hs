{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.Utils
  ( removeDuplicates,
    isEnum,
    getSource,
    handleResult,
    getFile,
    omitNulls,
    emptyTypeError,
    takeValueType,
    readSchemaSource,
    invalidConstructorError,
  )
where

import Control.Monad.Except (MonadError (catchError))
import Data.Aeson (Object)
import Data.Aeson.Types (Pair, Parser, Value (..), object)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.FileEmbed (makeRelativeToProject)
import Data.List (isSuffixOf)
import Data.Morpheus.Client.Fetch.Types
  ( SchemaSource (..),
  )
import Data.Morpheus.CodeGen.Internal.AST (CodeGenConstructor (..), CodeGenTypeName (..), getFullName)
import Data.Morpheus.Error (gqlWarnings, renderGQLErrors)
import Data.Morpheus.Internal.Ext (GQLResult, Result (..))
import Data.Morpheus.Internal.Utils (IsMap (lookup))
import Data.Morpheus.Types.Internal.AST (Msg (msg), internal)
import qualified Data.Text as T
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

isEnum :: [CodeGenConstructor] -> Bool
isEnum = all (null . constructorFields)

readSchemaSource :: FilePath -> IO SchemaSource
readSchemaSource p
  | ".json" `isSuffixOf` p = JSON <$> L.readFile p
  | ".gql" `isSuffixOf` p || ".graphql" `isSuffixOf` p = GQL <$> L.readFile p
  | otherwise = fail "Unsupported file format! The input should have one of the following extensions: json, gql, graphql"

getSource :: FilePath -> Q SchemaSource
getSource = readWith readSchemaSource

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

omitNulls :: [Pair] -> Value
omitNulls = object . filter notNull
  where
    notNull (_, Null) = False
    notNull _ = True

emptyTypeError :: MonadFail m => CodeGenTypeName -> m a
emptyTypeError name = fail $ show $ internal ("Type " <> msg (getFullName name) <> " Should Have at least one Constructor")

takeValueType :: ((String, Object) -> Parser a) -> Value -> Parser a
takeValueType f (Object hMap) = case lookup "__typename" hMap of
  Nothing -> fail "key \"__typename\" not found on object"
  Just (String x) -> f (T.unpack x, hMap)
  Just val ->
    fail $ "key \"__typename\" should be string but found: " <> show val
takeValueType _ _ = fail "expected Object"

invalidConstructorError :: (MonadFail m, Show a) => a -> m b
invalidConstructorError v = fail $ show v <> " is Not Valid Union Constructor"
