{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI.File where

import Data.ByteString.Lazy.Char8 (ByteString, unpack, writeFile)
import Data.Char
import Data.Morpheus.Internal.Ext
  ( GQLResult,
    Result (..),
  )
import Data.Morpheus.Types.Internal.AST (GQLError (..))
import Relude hiding (ByteString, writeFile)
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

printWarnings :: [GQLError] -> IO ()
printWarnings [] = pure ()
printWarnings warnings = traverse_ handleWarning warnings
  where
    handleWarning warning =
      putStr $
        "  \x1b[33m  warning: "
          <> toString (message warning)
          <> "\x1b[0m\n      "
          <> unpack (show $ concat $toList $ locations warning)
          <> "\n"

saveDocument :: FilePath -> GQLResult ByteString -> IO ()
saveDocument _ (Failure errors) = print errors
saveDocument output Success {result, warnings} = printWarnings warnings >> writeFile output result

getModuleNameByPath :: FilePath -> FilePath -> [Char]
getModuleNameByPath root path = intercalate "." $ splitDirectories $ dropExtensions $ makeRelative root path
