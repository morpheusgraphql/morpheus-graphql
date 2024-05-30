{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Format (formatTable) where

import Data.List (maximum)
import qualified Data.Text as T
import Relude

type Table = [[Text]]

getSizes :: Table -> [Int]
getSizes xs = map size (transpose xs)
  where
    size :: [Text] -> Int
    size = maximum . map T.length

printRow :: [Int] -> [Text] -> Text
printRow sizes ls =
  T.strip
    $ T.intercalate "  "
    $ zipWith (\item s -> T.justifyLeft s ' ' item) ls sizes

formatTable :: Table -> [Text]
formatTable deps = sort $ map (printRow (getSizes deps)) deps
