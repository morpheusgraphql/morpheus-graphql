{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.CodeGen.TH
  ( deriveIfNotDefined,
    declareIfNotDeclared,
  )
where

import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenType (..),
    CodeGenTypeName (..),
    TypeClassInstance (..),
    getFullName,
  )
import Data.Morpheus.CodeGen.TH (toName)
import Language.Haskell.TH
import Relude

isTypeDeclared :: CodeGenTypeName -> Q Bool
isTypeDeclared name = isJust <$> lookupTypeName (toString $ getFullName name)

isInstanceDefined :: Name -> CodeGenTypeName -> Q Bool
isInstanceDefined typeClass tName = do
  exists <- isTypeDeclared tName
  if exists
    then isInstance typeClass [ConT (toName tName)]
    else pure False

deriveIfNotDefined :: (TypeClassInstance a -> Q Dec) -> TypeClassInstance a -> Q [Dec]
deriveIfNotDefined f dec = do
  exists <- isInstanceDefined (typeClassName dec) (typeClassTarget dec)
  traverse f [dec | not exists]

declareIfNotDeclared :: (CodeGenType -> Q a) -> CodeGenType -> Q [a]
declareIfNotDeclared f codeGenType = do
  exists <- isTypeDeclared (cgTypeName codeGenType)
  traverse f [codeGenType | not exists]