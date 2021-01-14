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
  ( ClientConsD,
    ClientTypeDefinition (..),
    TypeNameTH (..),
  )
import Data.Morpheus.Client.Internal.Utils
  ( isEnum,
  )
import Data.Morpheus.Internal.TH
  ( declareTypeRef,
    nameSpaceType,
    toCon,
    toName,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ConsD (..),
    FieldDefinition (..),
    FieldName,
    TypeKind (..),
    TypeName,
    VALID,
  )
import Language.Haskell.TH
import Relude hiding (Type)

typeDeclarations :: TypeKind -> [ClientTypeDefinition -> Q Dec]
typeDeclarations KindScalar = []
typeDeclarations _ = [pure . declareType]

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

declareCons :: TypeNameTH -> [ClientConsD ANY] -> [Con]
declareCons TypeNameTH {namespace, typename} clientCons
  | isEnum clientCons = map consE clientCons
  | otherwise = map consR clientCons
  where
    consE ConsD {cName} = NormalC (mkConName namespace (typename <> cName)) []
    consR ConsD {cName, cFields} =
      RecC
        (mkConName namespace cName)
        (map declareField cFields)

declareField :: FieldDefinition ANY VALID -> (Name, Bang, Type)
declareField FieldDefinition {fieldName, fieldType} =
  ( toName fieldName,
    Bang NoSourceUnpackedness NoSourceStrictness,
    declareTypeRef toCon fieldType
  )

mkConName :: [FieldName] -> TypeName -> Name
mkConName namespace = toName . nameSpaceType namespace
