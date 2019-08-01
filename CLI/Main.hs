{-# LANGUAGE NamedFieldPuns #-}

module Main
  ( main
  ) where

import qualified Data.ByteString.Lazy   as L (readFile, writeFile)
import           Data.Semigroup         ((<>))
import           Options.Applicative    (command, customExecParser, fullDesc, help, helper, info, long, metavar, prefs,
                                         progDesc, short, showHelpOnError, subparser, switch)
import qualified Options.Applicative    as OA

-- MORPHEUS
import           Data.Morpheus.Document (toMorpheusHaskellAPi)

version :: String
version = "0.1.1"

main :: IO ()
main = defaultParser >>= writeHaskell
  where
    writeHaskell Options {optionCommand} = executeCommand optionCommand
      where
        executeCommand Version = putStrLn $ "Morpheus GraphQL CLI, version " <> version
        executeCommand Build {source, target} = toMorpheusHaskellAPi <$> L.readFile source >>= saveDocument
          where
            saveDocument (Left errors) = print errors
            saveDocument (Right doc)   = L.writeFile target doc

data Command
  = Build { source :: FilePath
          , target :: FilePath }
  | Version
  deriving (Show)

data Options = Options
  { optionVerbose :: Bool
  , optionCommand :: Command
  } deriving (Show)

defaultParser :: IO Options
defaultParser = customExecParser (prefs showHelpOnError) (info (helper <*> optionParser) morpheusDescription)
  where
    morpheusDescription = fullDesc <> progDesc "Morpheus GraphQL CLI - haskell Api Generator"
    -----------------------------------------------------
    optionParser :: OA.Parser Options
    optionParser = Options <$> verboseParser <*> commandParser
      where
        verboseParser = switch (long "version" <> short 'v' <> help "show Version number")
        ----------------------------------------------------------------------------------------------
        commandParser = subparser $ foldr ((<>) . produceCommand) mempty commands
          where
            pathParser label = OA.strArgument $ metavar label <> help (label <> " file")
            produceCommand (c, a, b) = command c (info (helper <*> a) b)
            commands =
              [ ( "build"
                , pure Build <*> pathParser "source" <*> pathParser "target"
                , fullDesc <> progDesc "generate hs files with schema.gql")
              , ("version", pure Version, fullDesc <> progDesc "Clean up")
              ]
