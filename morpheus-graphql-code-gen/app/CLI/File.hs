{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI.File where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
  ( writeFile,
  )
import Data.Char
import Data.Morpheus.Internal.Ext
  ( GQLResult,
    Result (..),
  )
import Relude hiding (ByteString)
import System.FilePath.Posix
  ( dropExtensions,
    makeRelative,
    normalise,
    replaceExtensions,
    splitDirectories,
    splitFileName,
    (</>),
  )

processFileName :: FilePath -> FilePath
processFileName = (\(x, y) -> x </> replaceExtensions (capitalize y) "hs") . splitFileName . normalise

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : xs

saveDocument :: FilePath -> GQLResult ByteString -> IO ()
saveDocument _ (Failure errors) = print errors
saveDocument output Success {result} = L.writeFile output result

getModuleNameByPath :: FilePath -> FilePath -> [Char]
getModuleNameByPath root path = intercalate "." $ splitDirectories $ dropExtensions $ makeRelative root path
