{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.MergeSchema
  ( test,
  )
where

import Control.Applicative (pure)
import Control.Monad ((>=>), (>>=))
import Control.Monad.Fail (fail)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    eitherDecode,
  )
import qualified Data.ByteString.Lazy as L (readFile)
import Data.Either (either)
import Data.Foldable (foldrM)
import Data.Functor (fmap)
import Data.Morpheus.Core
  ( parseGQLDocument,
    render,
  )
import Data.Morpheus.Internal.Utils ((<:>))
import Data.Morpheus.Types.Internal.AST
  ( Schema,
    VALID,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( resultOr,
  )
import Data.Morpheus.Types.Internal.Stitching (Stitching (stitch))
import Data.Semigroup ((<>))
import Data.Text (unpack)
import Data.Traversable (traverse)
import GHC.Generics (Generic)
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertFailure,
    testCase,
  )
import Prelude
  ( ($),
    (.),
    Bool (..),
    Eq (..),
    FilePath,
    IO,
    Maybe (..),
    otherwise,
    show,
  )

data Case = Case
  { schemas :: [FilePath],
    success :: Maybe Bool
  }
  deriving (Generic, FromJSON, ToJSON)

readSchema :: FilePath -> IO (Schema VALID)
readSchema =
  L.readFile . ("test/" <>) . (<> ".gql")
    >=> (resultOr (fail . show) pure . parseGQLDocument)

readSchemas :: Case -> IO [Schema VALID]
readSchemas = traverse readSchema . schemas

getCase :: FilePath -> IO Case
getCase url =
  L.readFile ("test/" <> url <> "/case.json")
    >>= either fail prefix . eitherDecode
  where
    prefix Case {..} = pure $ Case {schemas = fmap ((url <> "/") <>) schemas, ..}

readSchemaResult :: FilePath -> IO (Schema VALID)
readSchemaResult = readSchema . (<> "/success")

joinSchemas :: [Schema VALID] -> IO (Schema VALID)
joinSchemas [] = fail "no empty case"
joinSchemas (x : xs) = resultOr (fail . show) pure (foldrM stitch x xs)

schemaCase :: FilePath -> TestTree
schemaCase url = testCase url $ do
  schemas <- getCase url >>= readSchemas
  expected <- readSchemaResult url
  result <- joinSchemas schemas
  assertion expected result

assertion :: Schema VALID -> Schema VALID -> IO ()
assertion expectedSchema schema
  | render expectedSchema == render schema = pure ()
  | otherwise =
    assertFailure
      $ unpack
      $ "expected: \n " <> render expectedSchema <> " \n but got: \n " <> render schema

test :: TestTree
test =
  testGroup
    "merge schema"
    [ schemaCase "merge/schema/simple-query"
    ]
