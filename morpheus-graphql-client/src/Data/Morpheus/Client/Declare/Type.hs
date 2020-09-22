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

--
-- MORPHEUS
import Control.Applicative(pure)
import Data.Morpheus.Client.Internal.Types
  ( ClientTypeDefinition (..),
    TypeNameTH (..),
  )
import Data.Morpheus.Internal.TH
  ( declareTypeRef,
    isEnum,
    nameSpaceType,
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
import Data.Semigroup ((<>))
import GHC.Generics (Generic)
import Language.Haskell.TH
import Data.Maybe(Maybe(..))
import Prelude(
    Show,
    Eq,
    (.),
    otherwise,
    map
  )

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

declareCons :: TypeNameTH -> [ConsD ANY VALID] -> [Con]
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
    declareTypeRef fieldType
  )

mkConName :: [FieldName] -> TypeName -> Name
mkConName namespace = toName . nameSpaceType namespace
