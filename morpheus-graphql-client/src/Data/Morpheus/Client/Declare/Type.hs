{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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

typeDeclarations :: TypeKind -> ClientTypeDefinition -> Q [Dec]
typeDeclarations KindScalar _ = pure []
typeDeclarations _ c@ClientTypeDefinition{clientTypeName = TypeNameTH{namespace, typename}} = do
    let name = mkConName namespace typename
    m <- lookupTypeName (show name)
    case m of
        Nothing -> pure [declareType c]
        _ -> pure []

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
