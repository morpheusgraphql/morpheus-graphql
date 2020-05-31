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
    FieldName,
    TRUE,
    TypeKind (..),
    TypeName,
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
      (mkNamespace tNamespace tName)
      tVars
      Nothing
      cons
      (derive tKind)
  ]
  where
    tVars = declareTyVar (tyConArgs tKind)
      where
        declareTyVar = map (PlainTV . mkTypeName)
    cons = declareCons namespace tKind (tNamespace, tName) tCons

derive :: TypeKind -> [DerivClause]
derive tKind = [deriveClasses (''Generic : derivingList)]
  where
    derivingList
      | isOutput tKind = []
      | otherwise = [''Show]

deriveClasses :: [Name] -> DerivClause
deriveClasses classNames = DerivClause Nothing (map ConT classNames)

mkNamespace :: [FieldName] -> TypeName -> Name
mkNamespace tNamespace = mkTypeName . nameSpaceType tNamespace

declareCons ::
  Bool ->
  TypeKind ->
  ([FieldName], TypeName) ->
  [ConsD cat] ->
  [Con]
declareCons namespace tKind (tNamespace, tName) = map consR
  where
    consR ConsD {cName, cFields} =
      RecC
        (mkNamespace tNamespace cName)
        (map (declareField namespace tKind tName) cFields)

declareField ::
  Bool ->
  TypeKind ->
  TypeName ->
  FieldDefinition cat ->
  (Name, Bang, Type)
declareField namespace tKind tName field@FieldDefinition {fieldName} =
  ( fieldTypeName namespace tName fieldName,
    Bang NoSourceUnpackedness NoSourceStrictness,
    renderFieldType tKind field
  )

renderFieldType ::
  TypeKind ->
  FieldDefinition cat ->
  Type
renderFieldType tKind FieldDefinition {fieldContent, fieldType} =
  genFieldT
    (declareTypeRef (isSubscription tKind) fieldType)
    tKind
    fieldContent

fieldTypeName :: Bool -> TypeName -> FieldName -> Name
fieldTypeName namespace tName fieldName
  | namespace = mkFieldName (nameSpaceField tName fieldName)
  | otherwise = mkFieldName fieldName

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
