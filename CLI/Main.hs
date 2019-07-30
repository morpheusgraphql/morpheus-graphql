module Main
  ( main
  ) where

import qualified Options.Applicative as OA

data MorpheusArgs = MorpheusArgs
  { argVersion :: Bool
  , argFiles   :: [FilePath]
  } deriving (Show)

parserInfo :: OA.ParserInfo MorpheusArgs
parserInfo = OA.info (OA.helper <*> parseMorpheusArgs) $ OA.fullDesc <> OA.header "2.0.0"

parseMorpheusArgs :: OA.Parser MorpheusArgs
parseMorpheusArgs =
  MorpheusArgs <$> OA.switch (OA.help "Show version information" <> OA.long "version" <> OA.hidden) <*>
  OA.many (OA.strArgument $ OA.metavar "FILENAME" <> OA.help "Input file(s)")

main :: IO ()
main = OA.execParser parserInfo >>= print
