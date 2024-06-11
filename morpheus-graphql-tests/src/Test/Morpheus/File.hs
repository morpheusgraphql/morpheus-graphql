{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Morpheus.File
  ( withSource,
    ReadSource,
    cd,
    file,
    readJSON,
    readGQL,
    FileUrl (..),
    ls,
    mkUrl,
    isDirectory,
    scanDirectories,
    searchAppFiles,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import qualified Data.Text.IO as T
import Relude hiding (ByteString)
import System.Directory (doesDirectoryExist, listDirectory)

class ReadSource t where
  readSource :: (ToString name) => name -> IO t

instance ReadSource Text where
  readSource = T.readFile . toString

instance ReadSource ByteString where
  readSource = L.readFile . toString

withSource :: (ReadSource t) => (String, String) -> FileUrl -> IO t
withSource (name, format) url
  | isDir url = readSource $ toString url <> "/" <> name <> "." <> format
  | otherwise = readSource $ toString url <> "." <> format

readGQL :: (ReadSource t) => String -> FileUrl -> IO t
readGQL x = withSource (x, "gql")

readJSON :: (ReadSource t) => String -> FileUrl -> IO t
readJSON x = withSource (x, "json")

data FileUrl = FileUrl
  { filePath :: [FilePath],
    fileName :: FilePath,
    isDir :: Bool
  }
  deriving (Show)

instance ToString FileUrl where
  toString FileUrl {..} = foldl' (\y x -> x <> "/" <> y) fileName filePath

goTo :: FileUrl -> FilePath -> Bool -> FileUrl
goTo FileUrl {fileName, filePath} name isDir =
  FileUrl
    { filePath = fileName : filePath,
      fileName = name,
      ..
    }

cd :: FileUrl -> FilePath -> FileUrl
cd url name = goTo url name True

file :: FileUrl -> FilePath -> FileUrl
file url name = goTo url name False

ls :: FileUrl -> IO [FileUrl]
ls url = do
  files <- listDirectory (toString url)
  traverse mkFile files
  where
    mkFile name =
      goTo url name
        <$> doesDirectoryExist
          (toString url <> "/" <> name)

isDirectory :: FileUrl -> IO Bool
isDirectory = doesDirectoryExist . toString

mkUrl :: FilePath -> FileUrl
mkUrl fileName =
  FileUrl
    { filePath = ["test"],
      fileName,
      isDir = True
    }

scanDirectories :: FileUrl -> IO [FileUrl]
scanDirectories = fmap (filter isDir) . ls

searchAppFiles :: FileUrl -> IO [FilePath]
searchAppFiles = fmap (nub . mapMaybe isAppFile) . ls
  where
    isAppFile FileUrl {fileName, isDir}
      | not isDir
          && "app-"
          `isPrefixOf` fileName =
          Just $ takeWhile (/= '.') fileName
      | otherwise = Nothing
