{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Execution.Document.Compile
  ( compileDocument
  ) where

import qualified Data.Text                                    as T (pack)
import           Language.Haskell.TH

--
--  Morpheus
import           Data.Morpheus.Error.Client.Client            (renderGQLErrors)
import           Data.Morpheus.Execution.Document.Convert     (renderTHTypes)
import           Data.Morpheus.Execution.Document.Declare     (declareTypes)
import           Data.Morpheus.Parsing.Document.Parser        (parseTypes)
import           Data.Morpheus.Validation.Document.Validation

compileDocument :: Bool -> String -> Q [Dec]
compileDocument namespace documentTXT =
  case parseTypes (T.pack documentTXT) >>= validatePartialDocument >>= renderTHTypes namespace of
    Left errors  -> fail (renderGQLErrors errors)
    Right schema -> declareTypes namespace schema
