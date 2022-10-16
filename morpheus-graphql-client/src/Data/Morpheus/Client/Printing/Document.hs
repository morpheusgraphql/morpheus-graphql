{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Printing.Document
  ( printClientTypeDeclarations,
  )
where

import Data.Morpheus.Client.Internal.AST (ClientDeclaration)
import Prettyprinter (Doc)

printClientTypeDeclarations :: [ClientDeclaration] -> Doc n
printClientTypeDeclarations _ = "TODO: client"