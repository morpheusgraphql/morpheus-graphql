{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Morpheus.Utils
  ( getVariables,
    deepScan,
    FileUrl (..),
    CaseTree (..),
    ReadSource (..),
    scanDirectories,
    recursiveScan,
    assertEqualFailure,
    assertValidSchemaFailure,
    runCaseTree,
    getResponse,
    cd,
    getRequestBy,
    assertEqualResponse,
  )
where

import Data.Aeson (Value (..), decode, encode)
import qualified Data.ByteString.Lazy as L (readFile)
import qualified Data.ByteString.Lazy.Char8 as LB (unpack)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Text.IO as T
import Relude hiding (ByteString)
import System.Directory (doesDirectoryExist, listDirectory)
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertFailure,
  )

class ReadSource t where
  readQueryFile :: ToString name => name -> IO t
  readResponseFile :: ToString name => name -> IO t
  readSchemaFile :: ToString name => name -> IO t

instance ReadSource ByteString where
  readQueryFile x = L.readFile $ toString x <> "/query.gql"
  readResponseFile x = L.readFile $ toString x <> "/response.json"
  readSchemaFile x = L.readFile $ toString x <> "/schema.gql"

instance ReadSource Text where
  readQueryFile x = T.readFile $ toString x <> "/query.gql"
  readResponseFile x = T.readFile $ toString x <> "/response.json"
  readSchemaFile x = T.readFile $ toString x <> "/schema.gql"

runCaseTree :: (FileUrl -> [FileUrl] -> [TestTree]) -> CaseTree -> TestTree
runCaseTree f CaseTree {caseUrl, children = [], assets} =
  testGroup (fileName caseUrl) (f caseUrl assets)
runCaseTree f CaseTree {caseUrl = FileUrl {fileName}, children} =
  testGroup fileName (fmap (runCaseTree f) children)

getRequestBy :: (ReadSource a, ToString t) => (a -> Maybe Value -> b) -> t -> IO b
getRequestBy f url =
  f
    <$> readQueryFile url
    <*> getVariables url

getVariables :: ToString t => t -> IO (Maybe Value)
getVariables x = decode <$> (L.readFile (toString x <> "/variables.json") <|> pure "{}")

getResponse :: ToString t => t -> IO Value
getResponse p = fromMaybe Null . decode <$> readResponseFile p

data FileUrl = FileUrl
  { filePath :: [FilePath],
    fileName :: FilePath
  }
  deriving (Show)

data CaseTree = CaseTree
  { caseUrl :: FileUrl,
    children :: [CaseTree],
    assets :: [FileUrl]
  }
  deriving (Show)

cd :: FileUrl -> FilePath -> FileUrl
cd FileUrl {..} x =
  FileUrl
    { filePath = fileName : filePath,
      fileName = x
    }

instance ToString FileUrl where
  toString FileUrl {..} = foldl' (\y x -> x <> "/" <> y) fileName filePath

isDirectory :: FileUrl -> IO Bool
isDirectory = doesDirectoryExist . toString

scanDirectories :: FileUrl -> IO [FileUrl]
scanDirectories p = do
  list <- map (cd p) <$> listDirectory (toString p)
  filterM isDirectory list

deepScan :: FilePath -> IO CaseTree
deepScan = recursiveScan scanDirectories . FileUrl ["test"]

recursiveScan :: (FileUrl -> IO [FileUrl]) -> FileUrl -> IO CaseTree
recursiveScan scanAssets caseUrl = do
  dir <- isDirectory caseUrl
  children <-
    if dir
      then do
        list <- map (cd caseUrl) <$> listDirectory (toString caseUrl)
        areDirectories <- traverse isDirectory list
        if and areDirectories
          then traverse (recursiveScan scanAssets) list
          else pure []
      else pure []
  assets <- if dir && null children then scanAssets caseUrl else pure []
  pure CaseTree {..}

assertEqualResponse :: Value -> Value -> IO ()
assertEqualResponse expected actual
  | expected == actual = pure ()
  | otherwise =
    assertFailure
      $ LB.unpack
      $ "expected: \n\n "
        <> show (encode expected)
        <> " \n\n but got: \n\n "
        <> show (encode actual)

assertEqualFailure :: ByteString -> ByteString -> IO ()
assertEqualFailure expected actual
  | expected == actual = pure ()
  | otherwise =
    assertFailure
      $ LB.unpack
      $ "expected: \n\n "
        <> show expected
        <> " \n\n but got: \n\n "
        <> show actual

assertValidSchemaFailure :: Show err => err -> IO a
assertValidSchemaFailure err =
  assertFailure $
    "unexpected schema validation error: \n "
      <> show err
