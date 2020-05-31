{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.TH.Declare.Type
  ( declareType,
  )
where

import Data.Morpheus.Internal.TH
  ( declareTypeRef,
    m',
    mkFieldName,
    mkTypeName,
    nameSpaceField,
    nameSpaceType,
    tyConArgs,
  )
import Data.Morpheus.Server.Internal.TH.Types (ServerTypeDefinition (..))
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    ConsD (..),
    FieldContent (..),
    FieldDefinition (..),
    TRUE,
    TypeKind (..),
    isOutput,
    isOutputObject,
    isSubscription,
  )
import Data.Semigroup ((<>))
import GHC.Generics (Generic)
import Language.Haskell.TH

declareType :: Bool -> ServerTypeDefinition cat -> [Dec]
declareType _ ServerTypeDefinition {tKind = KindScalar} = []
declareType namespace ServerTypeDefinition {tName, tCons, tKind, tNamespace} =
  [ DataD
      []
      (genName tName)
      tVars
      Nothing
      cons
      [derive (''Generic : derivingList)]
  ]
  where
    derivingList
      | isOutput tKind = []
      | otherwise = [''Show]
    genName = mkTypeName . nameSpaceType tNamespace
    tVars = declareTyVar (tyConArgs tKind)
      where
        declareTyVar = map (PlainTV . mkTypeName)
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    derive classNames = DerivClause Nothing (map ConT classNames)
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
            fiType = genFieldT result tKind fieldContent
              where
                ---------------------------
                result = declareTypeRef (isSubscription tKind) fieldType

------------------------------------------------
genFieldT :: Type -> TypeKind -> FieldContent TRUE cat -> Type
genFieldT result _ (FieldArgs ArgumentsDefinition {argumentsTypename = Just argsTypename}) =
  AppT
    (AppT arrowType argType)
    (AppT m' result)
  where
    argType = ConT $ mkTypeName argsTypename
    arrowType = ConT ''Arrow
genFieldT result kind _
  | isOutputObject kind = AppT m' result
  | otherwise = result

type Arrow = (->)
