{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Declare.Type
  ( typeDeclarations,
  )
where

import Data.Morpheus.Client.Internal.Types
  ( ClientConstructorDefinition (..),
    ClientTypeDefinition (..),
    TypeNameTH (..),
  )
import Data.Morpheus.Client.Internal.Utils
  ( isEnum,
  )
import Data.Morpheus.CodeGen.Internal.TH
  ( camelCaseTypeName,
    declareTypeRef,
    toCon,
    toName,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FieldDefinition (..),
    FieldName,
    TypeKind (..),
    TypeName,
    VALID,
  )
import Language.Haskell.TH
import Relude hiding (Type)
import Data.Morpheus.Client.Internal.TH (isTypeDeclared)

typeDeclarations :: TypeKind -> ClientTypeDefinition -> Q [Dec]
typeDeclarations KindScalar _ = pure []
typeDeclarations _ c = do
    exists <- isTypeDeclared c
    if exists
        then pure []
        else pure [declareType c]

declareType :: ClientTypeDefinition -> Dec
declareType
  ClientTypeDefinition
    { clientTypeName = thName@TypeNameTH {namespace, typename},
      clientCons
    } =
    DataD
      []
      (mkConName namespace typename)
      []
      Nothing
      (declareCons thName clientCons)
      (map derive [''Generic, ''Show, ''Eq])
    where
      derive className = DerivClause Nothing [ConT className]

declareCons :: TypeNameTH -> [ClientConstructorDefinition] -> [Con]
declareCons TypeNameTH {namespace, typename} clientCons
  | isEnum clientCons = map consE clientCons
  | otherwise = map consR clientCons
  where
    consE ClientConstructorDefinition {cName} = NormalC (mkTypeName namespace typename cName) []
    consR ClientConstructorDefinition {cName, cFields} =
      RecC
        (mkConName namespace cName)
        (map declareField cFields)

declareField :: FieldDefinition ANY VALID -> (Name, Bang, Type)
declareField FieldDefinition {fieldName, fieldType} =
  ( toName fieldName,
    Bang NoSourceUnpackedness NoSourceStrictness,
    declareTypeRef toCon fieldType
  )

mkTypeName :: [FieldName] -> TypeName -> TypeName -> Name
mkTypeName namespace typename = mkConName namespace . camelCaseTypeName [typename]

mkConName :: [FieldName] -> TypeName -> Name
mkConName namespace = toName . camelCaseTypeName namespace
