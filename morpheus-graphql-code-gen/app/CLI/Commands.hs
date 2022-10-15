{-# LANGUAGE NoImplicitPrelude #-}

module CLI.Commands
  ( BuildOptions (..),
    GlobalOptions (..),
    App (..),
    Operation (..),
    parseCLI,
  )
where

import Options.Applicative
  ( Parser,
    command,
    customExecParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
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
    options :: GlobalOptions
  }
  deriving (Show)

newtype GlobalOptions = GlobalOptions
  { version :: Bool
  }
  deriving (Show)

data BuildOptions = BuildOptions
  { root :: String,
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

parseCLI :: IO App
parseCLI =
  customExecParser
    (prefs showHelpOnError)
    (info (helper <*> parseApp) description)

parseApp :: OA.Parser App
parseApp = App <$> commandParser <*> parseOptions

parseOptions :: Parser GlobalOptions
parseOptions = GlobalOptions <$> switch (long "version" <> short 'v' <> help "show Version number")

description :: OA.InfoMod a
description = fullDesc <> progDesc "Morpheus GraphQL CLI - haskell Api Generator"
