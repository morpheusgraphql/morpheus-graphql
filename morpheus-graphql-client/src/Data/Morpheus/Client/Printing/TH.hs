{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Printing.TH
  ( printDeclarations,
  )
where

import Data.Morpheus.Client.Internal.AST
  ( ClientDeclaration (..),
  )
import Data.Morpheus.Client.Internal.TH
  ( declareIfNotDeclared,
    deriveIfNotDefined,
  )
import Data.Morpheus.CodeGen.TH
  ( PrintDec (printDec),
  )
import Language.Haskell.TH (Dec, Q)
import Relude hiding (ToString, Type, toString)

printDeclarations :: [ClientDeclaration] -> Q [Dec]
printDeclarations clientType = concat <$> traverse typeDeclarations clientType

typeDeclarations :: ClientDeclaration -> Q [Dec]
typeDeclarations (InstanceDeclaration dec) = deriveIfNotDefined printDec dec
typeDeclarations (ClientTypeDeclaration c) = declareIfNotDeclared printDec c