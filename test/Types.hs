{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Case (..),
    Name,
    testWith,
  )
where

import Data.Aeson (FromJSON)
import Data.Text (Text, unpack)
import qualified Data.Text as T (concat)
import GHC.Generics
import Lib (getCases)
import Test.Tasty (TestTree, testGroup)

type Name = Text

data Case = Case
  { path :: Text,
    description :: String
  }
  deriving (Generic, FromJSON)

testWith :: (Case -> IO TestTree) -> Name -> IO TestTree
testWith f dir = do
  cases <- getCases (unpack dir)
  test <- sequence $ f <$> map (\x -> x {path = T.concat [dir, "/", path x]}) cases
  return $ testGroup (unpack dir) test
