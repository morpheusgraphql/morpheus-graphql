{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.Internal.TH.Types (declareType, TypeD (..)) where

import Data.Maybe (maybe)
-- MORPHEUS

import Data.Morpheus.Internal.TH
import Data.Morpheus.Internal.Utils
  ( nameSpaceField,
    nameSpaceType,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ConsD,
    DataEnumValue (..),
    Description,
    Directives,
    FieldContent,
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    IN,
    TypeDefinition,
    TypeKind,
    TypeName,
    TypeRef (..),
    VALID,
    hsTypeName,
    mockFieldDefinition,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    ConsD (..),
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    TypeKind (..),
    TypeKind (..),
    TypeName (..),
    TypeRef (..),
    TypeWrapper (..),
    convertToHaskellName,
    isEnum,
    isOutputObject,
    isSubscription,
    readName,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( UnSubResolver,
  )
import Data.Semigroup ((<>))
import Data.Text (unpack)
import GHC.Generics (Generic)
import Language.Haskell.TH

--- Core
data TypeD cat = TypeD
  { tName :: TypeName,
    tNamespace :: [FieldName],
    typeArgD :: [TypeD IN],
    tCons :: [ConsD cat],
    tKind :: TypeKind,
    tDescription :: Maybe Description,
    typeOriginal :: TypeDefinition ANY
  }
  deriving (Show)

declareType :: Bool -> Maybe TypeKind -> [Name] -> TypeD cat -> Dec
declareType namespace kindD derivingList TypeD {tName, tCons, tNamespace} =
  DataD [] (genName tName) tVars Nothing cons $
    map derive (''Generic : derivingList)
  where
    genName = mkTypeName . nameSpaceType tNamespace
    tVars = maybe [] (declareTyVar . tyConArgs) kindD
      where
        declareTyVar = map (PlainTV . mkTypeName)
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    derive className = DerivClause Nothing [ConT className]
    cons = map consR tCons
    consR ConsD {cName, cFields} =
      RecC
        (genName cName)
        (map declareField cFields)
      where
        declareField FieldDefinition {fieldName, fieldContent, fieldType} =
          (mkFieldName fName, defBang, fiType)
          where
            fName
              | namespace = nameSpaceField tName fieldName
              | otherwise = fieldName
            fiType = genFieldT result kindD fieldContent
              where
                ---------------------------
                result = declareTypeRef (maybe False isSubscription kindD) fieldType

------------------------------------------------
genFieldT :: Type -> Maybe TypeKind -> FieldContent cat -> Type
genFieldT result _ (FieldArgs ArgumentsDefinition {argumentsTypename = Just argsTypename}) =
  AppT
    (AppT arrowType argType)
    (AppT m' result)
  where
    argType = ConT $ mkTypeName argsTypename
    arrowType = ConT ''Arrow
genFieldT result kind _
  | (isOutputObject <$> kind) == Just True = AppT m' result
  | otherwise = result

type Arrow = (->)

m' :: Type
m' = VarT $ mkTypeName m_

m_ :: TypeName
m_ = "m"
