{-# LANGUAGE NoImplicitPrelude #-}

module CLI.Commands
  ( GlobalOptions (..),
    App (..),
    Command (..),
    parseCLI,
  )
where

import Options.Applicative
  ( Parser,
    command,
    customExecParser,
    flag,
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

data Command
  = Setup (Maybe String)
  | Next String Bool
  | About
  deriving (Show)

data App = App
  { operations :: Command,
    options :: GlobalOptions
  }
  deriving (Show)

newtype GlobalOptions = GlobalOptions
  { version :: Bool
  }
  deriving (Show)

commandParser :: Parser Command
commandParser =
  buildOperation
    [ ("setup", "builds Haskell code from GQL source", Setup <$> optional parseVersion),
      ("about", "api information", pure About),
      ("next", "next release", Next <$> parseVersion <*> boolFlag "breaking" 'b')
    ]

buildOperation :: [(String, String, Parser Command)] -> Parser Command
buildOperation xs = joinParsers $ map parseOperation xs

joinParsers :: [OA.Mod OA.CommandFields a] -> Parser a
joinParsers xs = subparser $ mconcat xs

parseOperation :: (String, String, Parser Command) -> OA.Mod OA.CommandFields Command
parseOperation (bName, bDesc, bValue) =
  command bName (info (helper <*> bValue) (fullDesc <> progDesc bDesc))

parseVersion :: Parser String
parseVersion = (strArgument . mconcat) [metavar "version", help "existing version"]

boolFlag :: String -> Char -> Parser Bool
boolFlag l s = flag False True (long l <> short s)

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
