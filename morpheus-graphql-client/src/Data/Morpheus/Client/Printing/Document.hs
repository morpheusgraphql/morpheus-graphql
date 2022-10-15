{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Printing.Document
  ( printClientTypeDeclarations,
  )
where

import Data.Morpheus.Client.Internal.AST (ClientTypeDefinition)
import Prettyprinter (Doc)

printClientTypeDeclarations :: [ClientTypeDefinition] -> Doc n
printClientTypeDeclarations _ = "TODO: client"