{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

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
import Options.Applicative
  ( Parser,
    ReadM,
    command,
    customExecParser,
    eitherReader,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    prefs,
    progDesc,
    short,
    showHelpOnError,
    strArgument,
    subparser,
    switch,
  )
import qualified Options.Applicative as OA
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

data Operation
  = Build {source :: [FilePath]}
  | About
  deriving (Show)

data App = App
  { operations :: Operation,
    options :: Options
  }
  deriving (Show)

data Options = Options
  { version :: Bool,
    root :: String,
    namespaces :: Bool
  }
  deriving (Show)

defaultParser :: IO App
defaultParser =
  customExecParser
    (prefs showHelpOnError)
    (info (helper <*> parseApp) description)

parseApp :: OA.Parser App
parseApp = App <$> commandParser <*> parseOptions

parseOptions :: Parser Options
parseOptions =
  Options
    <$> switch (long "version" <> short 'v' <> help "show Version number")
    <*> option readOutput (long "root" <> short 'r' <> help "Root directory of the Haskell project")
    <*> switch (long "namespaces" <> short 'n' <> help "namespaces type fields to avoid name conflicts")

readOutput :: ReadM String
readOutput = eitherReader Right

description :: OA.InfoMod a
description = fullDesc <> progDesc "Morpheus GraphQL CLI - haskell Api Generator"

commandParser :: Parser Operation
commandParser =
  buildOperation
    [ ( "build",
        "builds Haskell API from GraphQL schema",
        Build <$> readFiles
      ),
      ( "about",
        "api information",
        pure About
      )
    ]

buildOperation :: [(String, String, Parser Operation)] -> Parser Operation
buildOperation xs = joinParsers $ map parseOperation xs

joinParsers :: [OA.Mod OA.CommandFields a] -> Parser a
joinParsers xs = subparser $ mconcat xs

parseOperation :: (String, String, Parser Operation) -> OA.Mod OA.CommandFields Operation
parseOperation (bName, bDesc, bValue) =
  command bName (info (helper <*> bValue) (fullDesc <> progDesc bDesc))

readFiles :: Parser [String]
readFiles =
  (many . strArgument . mconcat)
    [ metavar "file",
      help "source files for generating api"
    ]
