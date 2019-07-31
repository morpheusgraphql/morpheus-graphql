module Main
  ( main
  ) where

import qualified Data.ByteString.Lazy   as L (readFile, writeFile)
import           Data.Semigroup         ((<>))
import qualified Options.Applicative    as OA

-- MORPHEUS
import           Data.Morpheus.Document (toMorpheusHaskellAPi)

data MorpheusArgs = MorpheusArgs
  { argVersion :: Bool
  , argFiles   :: [FilePath]
  } deriving (Show)

parserInfo :: OA.ParserInfo MorpheusArgs
parserInfo = OA.info (OA.helper <*> parseMorpheusArgs) $ OA.header "2.0.0"

parseMorpheusArgs :: OA.Parser MorpheusArgs
parseMorpheusArgs =
  MorpheusArgs <$> OA.switch (OA.help "Show version information" <> OA.long "version" <> OA.hidden) <*>
  OA.many (OA.strArgument $ OA.metavar "FILENAME" <> OA.help "Input file(s)")

main :: IO ()
main = OA.execParser parserInfo >>= writeHaskell
  where
    writeHaskell MorpheusArgs {argFiles = [readPath, savePath]} =
      toMorpheusHaskellAPi <$> L.readFile readPath >>= saveDocument
      where
        saveDocument (Left errors) = print errors
        saveDocument (Right doc)   = L.writeFile savePath doc
    writeHaskell _ = print "Error: missing arguments"
