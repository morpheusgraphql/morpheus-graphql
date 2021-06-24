{-# LANGUAGE DeriveGeneric #-}
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
    getRequest,
    assertEqualResponse,
    foldCaseTree,
    assertResponse,
    getResolverValues,
    Response (..),
    responseAssertion,
    readResponse,
    renderingAssertion,
    assertValidSchema,
    select,
    readSchemaFile,
    readQueryFile,
    readResponseFile,
    readRenderingFile,
    getSchema,
    testApi,
  )
where

import Data.Aeson
  ( (.:),
    (.=),
    FromJSON (..),
    Result (..),
    ToJSON (..),
    Value (..),
    decode,
    eitherDecode,
    encode,
    fromJSON,
    object,
  )
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.HashMap.Lazy (lookup)
import qualified Data.Text.IO as T
import Relude hiding (ByteString)
import System.Directory (doesDirectoryExist, listDirectory)
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertFailure,
    testCase,
  )

class ReadSource t where
  readSource :: ToString name => name -> IO t

instance ReadSource Text where
  readSource = T.readFile . toString

instance ReadSource ByteString where
  readSource = L.readFile . toString

readQueryFile :: (ToString name, ReadSource t) => name -> IO t
readQueryFile x = readSource $ toString x <> "/query.gql"

readResponseFile :: (ToString name, ReadSource t) => name -> IO t
readResponseFile x = readSource $ toString x <> "/response.json"

readSchemaFile :: ReadSource t => FileUrl -> IO t
readSchemaFile x
  | isDir x =
    readSource $ toString x <> "/schema.gql"
  | otherwise = readSource $ toString x <> ".gql"

readResolversFile :: ReadSource t => FileUrl -> IO t
readResolversFile x
  | isDir x =
    readSource $ toString x <> "/resolvers.json"
  | otherwise = readSource $ toString x <> ".json"

readRenderingFile :: (ToString name, ReadSource t) => name -> IO t
readRenderingFile x = readSource $ toString x <> "/rendering.gql"

runCaseTree :: (FileUrl -> [FileUrl] -> [TestTree]) -> CaseTree -> TestTree
runCaseTree f CaseTree {caseUrl, children = [], assets} =
  testGroup (fileName caseUrl) (f caseUrl assets)
runCaseTree f CaseTree {caseUrl = FileUrl {fileName}, children} =
  testGroup fileName (fmap (runCaseTree f) children)

foldCaseTree :: (FileUrl -> TestTree) -> CaseTree -> TestTree
foldCaseTree f CaseTree {caseUrl, children = []} = f caseUrl
foldCaseTree f CaseTree {caseUrl = FileUrl {fileName}, children} =
  testGroup fileName (fmap (foldCaseTree f) children)

assertResponseFile :: ToJSON a => (FileUrl -> IO a) -> FileUrl -> TestTree
assertResponseFile f url =
  testCase (fileName url) $ f url >>= assertResponse url

assertResponse :: (ToString t, ToJSON a) => t -> a -> IO ()
assertResponse url response = do
  expectedResponse <- getResponse url
  assertEqualResponse expectedResponse (toJSON response)

getRequest :: (FromJSON req, ToString t) => t -> IO req
getRequest url = getRequestValue url >>= runResult . fromJSON
  where
    runResult (Success x) = pure x
    runResult (Error x) = fail x

getRequestValue :: (ToString t) => t -> IO Value
getRequestValue url = do
  (query :: Text) <- readQueryFile url
  variables <- getVariables url
  pure
    $ Object
    $ fromList
      [ "query" .= String query,
        "variables" .= variables
      ]

getVariables :: ToString t => t -> IO (Maybe Value)
getVariables x = decode <$> (L.readFile (toString x <> "/variables.json") <|> pure "{}")

getResponse :: ToString t => t -> IO Value
getResponse p = fromMaybe Null . decode <$> readResponseFile p

data FileUrl = FileUrl
  { filePath :: [FilePath],
    fileName :: FilePath,
    isDir :: Bool
  }
  deriving (Show)

data CaseTree = CaseTree
  { caseUrl :: FileUrl,
    children :: [CaseTree],
    assets :: [FileUrl]
  }
  deriving (Show)

cd :: FileUrl -> FilePath -> FileUrl
cd url name = goTo url name True

select :: FileUrl -> FilePath -> FileUrl
select url name = goTo url name False

goTo :: FileUrl -> FilePath -> Bool -> FileUrl
goTo FileUrl {fileName, filePath} name isDir =
  FileUrl
    { filePath = fileName : filePath,
      fileName = name,
      ..
    }

instance ToString FileUrl where
  toString FileUrl {..} = foldl' (\y x -> x <> "/" <> y) fileName filePath

isDirectory :: FileUrl -> IO Bool
isDirectory = doesDirectoryExist . toString

scanDirectories :: FileUrl -> IO [FileUrl]
scanDirectories = fmap (filter isDir) . ls

deepScan :: FilePath -> IO CaseTree
deepScan fileName =
  recursiveScan
    scanDirectories
    FileUrl
      { filePath = ["test"],
        fileName,
        isDir = True
      }

ls :: FileUrl -> IO [FileUrl]
ls url = do
  files <- listDirectory (toString url)
  traverse mkFile files
  where
    mkFile name = goTo url name <$> doesDirectoryExist (toString url <> "/" <> name)

recursiveScan :: (FileUrl -> IO [FileUrl]) -> FileUrl -> IO CaseTree
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
  assets <- if dir && null children then scanAssets caseUrl else pure []
  pure CaseTree {..}

assertEqualResponse :: Value -> Value -> IO ()
assertEqualResponse expected actual
  | expected == actual = pure ()
  | otherwise =
    assertFailure $
      "expected: \n\n "
        <> show (encode expected)
        <> " \n\n but got: \n\n "
        <> show (encode actual)

assertEqualFailure :: ByteString -> ByteString -> IO ()
assertEqualFailure expected actual
  | expected == actual = pure ()
  | otherwise =
    assertFailure $
      "expected: \n\n "
        <> show expected
        <> " \n\n but got: \n\n "
        <> show actual

getSchema :: (ReadSource a, Show err) => (a -> Either err b) -> FileUrl -> IO b
getSchema f url =
  readSchemaFile url
    >>= assertValidSchema . f

assertValidSchema :: Show err => Either err a -> IO a
assertValidSchema = either assertValidSchemaFailure pure

assertValidSchemaFailure :: Show err => err -> IO a
assertValidSchemaFailure err =
  assertFailure $
    "unexpected schema validation error: \n "
      <> show err

getResolverValues :: FileUrl -> IO (Value, Value, Value)
getResolverValues url = do
  res <- fromMaybe Null . decode <$> readResolversFile url
  pure
    ( lookupRes "query" res,
      lookupRes "mutation" res,
      lookupRes "subscription" res
    )

lookupRes :: Text -> Value -> Value
lookupRes name (Object fields) = fromMaybe Null (lookup name fields)
lookupRes _ _ = Null

data Response e
  = OK
  | ExpectedErrors e
  | UnexpectedError String
  deriving (Generic)

instance (FromJSON e) => FromJSON (Response e) where
  parseJSON (Object v) =
    ExpectedErrors <$> v .: "errors"
  parseJSON (String "OK") = pure OK
  parseJSON v = pure $ UnexpectedError (show v)

instance (ToJSON e) => ToJSON (Response e) where
  toJSON OK = String "OK"
  toJSON (ExpectedErrors err) = object ["errors" .= toJSON err]
  toJSON (UnexpectedError err) = String (fromString err)

responseAssertion :: (Eq err, ToJSON err) => Response err -> Either err a -> IO ()
responseAssertion OK Right {} = pure ()
responseAssertion (ExpectedErrors err) (Left errors)
  | err == errors =
    pure
      ()
responseAssertion expected Right {} = assertEqualFailure (encode expected) "OK"
responseAssertion expected (Left errors) =
  assertEqualFailure
    (encode expected)
    (encode (ExpectedErrors errors))

readResponse :: (ToString t, FromJSON a) => t -> IO (Response a)
readResponse = fmap (either UnexpectedError id . eitherDecode) . readResponseFile

renderingAssertion ::
  Show e =>
  (FileUrl -> IO (Either e ByteString)) ->
  FileUrl ->
  TestTree
renderingAssertion api url = testCase (fileName url) $ do
  actual <- api url
  expected <- readRenderingFile url
  either
    (assertFailure . (" error: " <>) . show)
    (assertEqualFailure expected)
    actual

testApi :: (ToJSON res, FromJSON req) => (req -> IO res) -> FileUrl -> TestTree
testApi api = assertResponseFile (getRequest >=> api)
