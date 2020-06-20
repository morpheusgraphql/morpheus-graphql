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
    FileUrl (..),
    CaseTree (..),
    toString,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, Value (..), decode)
import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromMaybe)
import Data.Morpheus.Types.Internal.AST (FieldName (..))
import Data.Semigroup ((<>))
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

data CaseTree = CaseTree
  { caseUrl :: FileUrl,
    children :: Either [String] [CaseTree]
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

scanSchemaTests :: FilePath -> IO CaseTree
scanSchemaTests = deepScan

deepScan :: FilePath -> IO CaseTree
deepScan = shouldScan . FileUrl []
  where
    shouldScan :: FileUrl -> IO CaseTree
    shouldScan caseUrl@FileUrl {..} = do
      children <- prefixed caseUrl
      pure CaseTree {..}
    isDirectory :: FileUrl -> IO Bool
    isDirectory = fmap (fileTypeIsDirectory . fileTypeFromMetadata) . getFileMetadata . toString
    prefixed :: FileUrl -> IO (Either [String] [CaseTree])
    prefixed p = do
      dir <- isDirectory p
      if dir
        then do
          list <- map (prefix p) <$> listDirectory (toString p)
          areDirectories <- traverse isDirectory list
          if and areDirectories
            then Right <$> traverse shouldScan list
            else pure $ Left []
        else pure $ Left []

maybeVariables :: FieldName -> IO (Maybe Value)
maybeVariables (FieldName x) = decode <$> (L.readFile (path x ++ "/variables.json") <|> return "{}")

getGQLBody :: FieldName -> IO ByteString
getGQLBody (FieldName p) = L.readFile (gqlLib p)

getCases :: FromJSON a => String -> IO [a]
getCases dir = fromMaybe [] . decode <$> L.readFile ("test/" ++ dir ++ "/cases.json")

getResponseBody :: FieldName -> IO Value
getResponseBody (FieldName p) = fromMaybe Null . decode <$> L.readFile (resLib p)
