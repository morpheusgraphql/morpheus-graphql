{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.TH.Declare.Type (declareType) where

import Data.Morpheus.Internal.TH
  ( declareTypeRef,
    m',
    mkFieldName,
    mkTypeName,
    nameSpaceField,
    nameSpaceType,
    tyConArgs,
  )
import Data.Morpheus.Server.Internal.TH.Types (TypeD (..))
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    ConsD (..),
    FieldContent (..),
    FieldDefinition (..),
    TRUE,
    TypeKind,
    isOutputObject,
    isSubscription,
  )
import Data.Semigroup ((<>))
import GHC.Generics (Generic)
import Language.Haskell.TH

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
genFieldT :: Type -> Maybe TypeKind -> FieldContent TRUE cat -> Type
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
