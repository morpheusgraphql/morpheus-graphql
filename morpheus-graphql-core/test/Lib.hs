{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( getGQLBody,
    getResponseBody,
    getCases,
    maybeVariables,
    readSource,
    scanSchemaTests,
  )
where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Data.Aeson (FromJSON, Value (..), decode)
import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromMaybe)
import Data.Morpheus.Types.Internal.AST (FieldName (..))
import Data.Text (Text, unpack)
import System.Directory (listDirectory)
import System.Directory.Internal
  ( fileTypeFromMetadata,
    fileTypeIsDirectory,
    getFileMetadata,
  )

readSource :: FieldName -> IO ByteString
readSource = L.readFile . path . readName

path :: Text -> String
path name = "test/" ++ unpack name

gqlLib :: Text -> String
gqlLib x = path x ++ "/query.gql"

resLib :: Text -> String
resLib x = path x ++ "/response.json"

data FileUrl = FileUrl
  { filePath :: [FilePath],
    fileName :: FilePath
  }
  deriving (Show)

prefix :: FileUrl -> FilePath -> FileUrl
prefix FileUrl {..} x =
  FileUrl
    { filePath = fileName : filePath,
      fileName = x
    }

toString :: FileUrl -> FilePath
toString FileUrl {..} = foldl (\y x -> x <> "/" <> y) fileName filePath

scanSchemaTests :: [FilePath] -> IO [FileUrl]
scanSchemaTests = fmap (concatMap selectTests) . deepScan
  where
    selectTests file
      | fileName file == "schema.gql" = [file]
      | otherwise = []

deepScan :: [FilePath] -> IO [FileUrl]
deepScan = fmap fst . shouldScan . ([],) . map (FileUrl [])
  where
    shouldScan :: ([FileUrl], [FileUrl]) -> IO ([FileUrl], [FileUrl])
    shouldScan (found, []) = pure (found, [])
    shouldScan (found, xs) = findNext xs >>= \x -> shouldScan (found <> x, x)
      where
        findNext = fmap concat . traverse prefixed
    isDirectory :: FileUrl -> IO Bool
    isDirectory = fmap (fileTypeIsDirectory . fileTypeFromMetadata) . getFileMetadata . toString
    prefixed :: FileUrl -> IO [FileUrl]
    prefixed p = do
      dir <- isDirectory p
      if dir
        then map (prefix p) <$> listDirectory (toString p)
        else pure []

maybeVariables :: FieldName -> IO (Maybe Value)
maybeVariables (FieldName x) = decode <$> (L.readFile (path x ++ "/variables.json") <|> return "{}")

getGQLBody :: FieldName -> IO ByteString
getGQLBody (FieldName p) = L.readFile (gqlLib p)

getCases :: FromJSON a => String -> IO [a]
getCases dir = fromMaybe [] . decode <$> L.readFile ("test/" ++ dir ++ "/cases.json")

getResponseBody :: FieldName -> IO Value
getResponseBody (FieldName p) = fromMaybe Null . decode <$> L.readFile (resLib p)
