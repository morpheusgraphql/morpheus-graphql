{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Morpheus.Utils
  ( FileUrl,
    deepScan,
    scan,
    getResolver,
    getSchema,
    requireEq,
    readSchemaFile,
  )
where

import Data.Aeson
  ( FromJSON (..),
    eitherDecode,
  )
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Relude hiding (ByteString)
import Test.Morpheus.File
  ( FileUrl (FileUrl, fileName, isDir),
    ReadSource,
    isDirectory,
    ls,
    readGQL,
    readJSON,
    scanDirectories,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertFailure,
  )

readSchemaFile :: (ReadSource t) => FileUrl -> IO t
readSchemaFile = readGQL "schema"

runCaseTree :: (FileUrl -> [FileUrl] -> [TestTree]) -> CaseTree [FileUrl] -> TestTree
runCaseTree f CaseTree {caseUrl, children = [], assets} =
  testGroup (fileName caseUrl) (f caseUrl assets)
runCaseTree f CaseTree {caseUrl = FileUrl {fileName}, children} =
  testGroup fileName (fmap (runCaseTree f) children)

foldCaseTree :: (FileUrl -> TestTree) -> CaseTree () -> TestTree
foldCaseTree f CaseTree {caseUrl, children = []} = f caseUrl
foldCaseTree f CaseTree {caseUrl = FileUrl {fileName}, children} =
  testGroup fileName (fmap (foldCaseTree f) children)

recursiveScan :: (Monoid assets) => (FileUrl -> IO assets) -> FileUrl -> IO (CaseTree assets)
recursiveScan scanAssets caseUrl = do
  dir <- isDirectory caseUrl
  children <-
    if dir
      then do
        list <- ls caseUrl
        if all isDir list
          then traverse (recursiveScan scanAssets) list
          else pure []
      else pure []
  assets <- if dir && null children then scanAssets caseUrl else pure mempty
  pure CaseTree {..}

scan :: (FileUrl -> TestTree) -> FileUrl -> IO TestTree
scan f url = foldCaseTree f <$> recursiveScan (const (pure ())) url

deepScan :: (FileUrl -> [FileUrl] -> [TestTree]) -> FileUrl -> IO TestTree
deepScan f url = runCaseTree f <$> recursiveScan scanDirectories url

data CaseTree assets = CaseTree
  { caseUrl :: FileUrl,
    children :: [CaseTree assets],
    assets :: assets
  }
  deriving (Show)

requireEq :: (Eq t) => (t -> ByteString) -> t -> t -> IO ()
requireEq f expected actual
  | expected == actual = pure ()
  | otherwise = eqFailureMessage (f expected) (f actual)

eqFailureMessage :: ByteString -> ByteString -> IO a3
eqFailureMessage expected actual =
  assertFailure
    $ L.unpack
    $ "expected: \n\n "
    <> expected
    <> " \n\n but got: \n\n "
    <> actual

getSchema :: (ReadSource a, Show err) => (a -> Either err b) -> FileUrl -> IO b
getSchema f url =
  readSchemaFile url
    >>= assertValidSchema
    . f

assertValidSchema :: (Show err) => Either err a -> IO a
assertValidSchema =
  either
    ( assertFailure
        . ( "unexpected schema validation error: \n "
              <>
          )
        . show
    )
    pure

getResolver :: (FromJSON resolver) => FileUrl -> IO resolver
getResolver url = readJSON "resolvers" url >>= either fail pure . eitherDecode
