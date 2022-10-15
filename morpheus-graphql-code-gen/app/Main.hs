{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import CLI.Commands
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
  ( readFile,
    writeFile,
  )
import Data.Char
import Data.Morpheus.CodeGen
  ( CodeGenConfig (..),
    PrinterConfig (..),
    parseServerTypeDefinitions,
    printServerTypeDefinitions,
  )
import Data.Morpheus.Internal.Ext
  ( GQLResult,
    Result (..),
  )
import Data.Version (showVersion)
import qualified Paths_morpheus_graphql_code_gen as CLI
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

currentVersion :: String
currentVersion = showVersion CLI.version

main :: IO ()
main = defaultParser >>= runApp

runApp :: App -> IO ()
runApp App {..}
  | version options = putStrLn currentVersion
  | otherwise = runOperation operations
  where
    runOperation About =
      putStrLn $ "Morpheus GraphQL CLI, version " <> currentVersion
    runOperation Build {source} = traverse_ (processFile options) source

processFile :: Options -> FilePath -> IO ()
processFile Options {root, namespaces} path =
  print (path, hsPath)
    >> L.readFile path
    >>= saveDocument hsPath
      . fmap (printServerTypeDefinitions PrinterConfig {moduleName})
      . parseServerTypeDefinitions CodeGenConfig {namespace = namespaces}
  where
    hsPath = processFileName path
    moduleName = intercalate "." $ splitDirectories $ dropExtensions $ makeRelative root hsPath

processFileName :: FilePath -> FilePath
processFileName = (\(x, y) -> x </> replaceExtensions (capitalize y) "hs") . splitFileName . normalise

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : xs

saveDocument :: FilePath -> GQLResult ByteString -> IO ()
saveDocument _ (Failure errors) = print errors
saveDocument output Success {result} = L.writeFile output result
