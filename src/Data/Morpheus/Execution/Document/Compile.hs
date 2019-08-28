{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Execution.Document.Compile
  ( compileExp
  , compileDec
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

compileExp :: String -> Q Exp
compileExp documentTXT =
  case parseTypes (T.pack documentTXT) >>= validatePartialDocument >>= renderTHTypes of
    Left errors -> fail (renderGQLErrors errors)
    Right root  -> [|root|]

compileDec :: String -> Q [Dec]
compileDec documentTXT =
  case parseTypes (T.pack documentTXT) >>= validatePartialDocument >>= renderTHTypes of
    Left errors -> fail (renderGQLErrors errors)
    Right root  -> declareTypes root
