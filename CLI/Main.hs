{-# LANGUAGE NamedFieldPuns #-}

module Main
  ( main
  ) where

import qualified Data.ByteString.Lazy   as L (readFile, writeFile)
import           Data.Semigroup         ((<>))
import           Data.Version           (showVersion)
import           Options.Applicative    (Parser, command, customExecParser, fullDesc, help, helper, info, long, metavar,
                                         prefs, progDesc, short, showHelpOnError, strArgument, subparser, switch)
import qualified Options.Applicative    as OA
import           Paths_morpheus_graphql (version)

-- MORPHEUS
import           Data.Morpheus.Document (toMorpheusHaskellAPi)

morpheusVersion :: String
morpheusVersion = showVersion version

main :: IO ()
main = defaultParser >>= buildHaskellApi
  where
    buildHaskellApi Options {optionCommand} = executeCommand optionCommand
      where
        executeCommand About = putStrLn $ "Morpheus GraphQL CLI, version " <> morpheusVersion
        executeCommand Build {source, target} = toMorpheusHaskellAPi "Schema" <$> L.readFile source >>= saveDocument
          where
            saveDocument (Left errors) = print errors
            saveDocument (Right doc)   = L.writeFile target doc

data Command
  = Build { source :: FilePath
          , target :: FilePath }
  | About
  deriving (Show)

data Options = Options
  { optionVerbose :: Bool
  , optionCommand :: Command
  } deriving (Show)

data Behavior = Behavior
  { bName  :: String
  , bValue :: Parser Command
  , bDesc  :: String
  }

defaultParser :: IO Options
defaultParser = customExecParser (prefs showHelpOnError) (info (helper <*> optionParser) morpheusDescription)
  where
    morpheusDescription = fullDesc <> progDesc "Morpheus GraphQL CLI - haskell Api Generator"
    -----------------------------------------------------
    optionParser :: OA.Parser Options
    optionParser = Options <$> versionParser <*> commandParser
      where
        versionParser = switch (long "version" <> short 'v' <> help "show Version number")
        ----------------------------------------------------------------------------------------------
        commandParser = subparser $ foldr ((<>) . produceCommand) mempty commands
          where
            pathParser label = strArgument $ metavar label <> help (label <> " file")
            produceCommand Behavior {bName, bValue, bDesc} =
              command bName (info (helper <*> bValue) (fullDesc <> progDesc bDesc))
            commands =
              [ Behavior
                  { bName = "build"
                  , bValue = pure Build <*> pathParser "Source.gql" <*> pathParser "Target.hs"
                  , bDesc = "builds haskell API from  from GhraphQL schema \"*.gql\"  "
                  }
              , Behavior {bName = "about", bValue = pure About, bDesc = "api information"}
              ]
