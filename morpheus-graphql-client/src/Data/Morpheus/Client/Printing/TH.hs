{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Printing.TH
  ( printDeclarations,
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
  )
import Data.Morpheus.Client.Fetch.RequestType
  ( RequestType (..),
  )
import Data.Morpheus.Client.Internal.AST
  ( ClientDeclaration (..),
    ClientMethod (..),
    ClientPreDeclaration (..),
    DERIVING_MODE (..),
    RequestTypeDefinition (..),
  )
import Data.Morpheus.Client.Internal.TH
  ( declareIfNotDeclared,
    deriveIfNotDefined,
    fromJSONEnumMethod,
    fromJSONObjectMethod,
    fromJSONUnionMethod,
  )
import Data.Morpheus.Client.Internal.Utils
  ( emptyTypeError,
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( AssociatedType (AssociatedTypeName),
    CodeGenType (..),
    MethodArgument (..),
    TypeClassInstance (..),
    fromTypeName,
  )
import Data.Morpheus.CodeGen.TH
  ( PrintDec (printDec),
    ToName (toName),
  )
import Data.Morpheus.Types.GQLScalar
  ( scalarFromJSON,
    scalarToJSON,
  )
import Language.Haskell.TH (Dec, Q)
import Relude hiding (ToString, Type, toString)

printDeclarations :: [ClientDeclaration] -> Q [Dec]
printDeclarations clientType = concat <$> traverse typeDeclarations clientType

typeDeclarations :: ClientDeclaration -> Q [Dec]
typeDeclarations (InstanceDeclaration dec) = deriveIfNotDefined printDec dec
typeDeclarations (ClientTypeDeclaration c) = declareIfNotDeclared printDec c