{-# LANGUAGE NoImplicitPrelude #-}

module CLI.Commands where

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
import Relude hiding (ByteString)

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
