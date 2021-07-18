{-# LANGUAGE GADTs #-}

module Data.Morpheus.Server.Deriving.Schema.Union
  ( buildUnionTypeContent,
  )
where

import Data.List (partition)
import Data.Morpheus.Internal.Utils (fromElems)
import Data.Morpheus.Server.Deriving.Schema.Enum
  ( defineEnumUnit,
  )
import Data.Morpheus.Server.Deriving.Schema.Object
  ( defineObjectType,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( ConsRep (..),
    fieldTypeName,
    isEmptyConstraint,
    isUnionRef,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType,
    TypeData (gqlTypeName),
    __typeData,
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    FieldContent (..),
    IN,
    TRUE,
    TypeContent (..),
    TypeName,
    UnionMember (..),
    mkNullaryMember,
    mkUnionMember,
  )
import Data.Morpheus.Utils.Kinded
  ( CategoryValue,
    KindedType (..),
  )
import Relude

buildUnionTypeContent ::
  ( GQLType a,
    CategoryValue kind
  ) =>
  KindedType kind a ->
  [ConsRep (Maybe (FieldContent TRUE kind CONST))] ->
  SchemaT c (TypeContent TRUE kind CONST)
buildUnionTypeContent scope cons = mkUnionType scope unionRef unionCons
  where
    unionRef = fieldTypeName <$> concatMap consFields unionRefRep
    (unionRefRep, unionCons) = partition (isUnionRef (gqlTypeName (__typeData scope))) cons

mkUnionType ::
  GQLType a =>
  KindedType kind a ->
  [TypeName] ->
  [ConsRep (Maybe (FieldContent TRUE kind CONST))] ->
  SchemaT c (TypeContent TRUE kind CONST)
mkUnionType p@InputType unionRef unionCons = DataInputUnion <$> (typeMembers >>= fromElems)
  where
    (nullaryCons, cons) = partition isEmptyConstraint unionCons
    nullaryMembers :: [UnionMember IN CONST]
    nullaryMembers = mkNullaryMember . consName <$> nullaryCons
    defineEnumEmpty
      | null nullaryCons = pure ()
      | otherwise = defineEnumUnit
    typeMembers =
      (<> nullaryMembers) . withRefs
        <$> ( defineEnumEmpty *> buildUnions p cons
            )
      where
        withRefs = fmap mkUnionMember . (unionRef <>)
mkUnionType p@OutputType unionRef unionCons =
  DataUnion <$> (buildUnions p unionCons >>= fromElems . map mkUnionMember . (unionRef <>))

buildUnions ::
  KindedType kind a ->
  [ConsRep (Maybe (FieldContent TRUE kind CONST))] ->
  SchemaT c [TypeName]
buildUnions proxy cons =
  traverse_ (defineObjectType proxy) cons $> fmap consName cons
