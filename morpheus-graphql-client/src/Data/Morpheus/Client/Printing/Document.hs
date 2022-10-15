module Data.Morpheus.Client.Printing.Document
  ( printClientTypeDeclarations,
  )
where

import Data.Morpheus.Client.Internal.Types (ClientTypeDefinition)

printClientTypeDeclarations :: [ClientTypeDefinition] -> [Char]
printClientTypeDeclarations x = ""