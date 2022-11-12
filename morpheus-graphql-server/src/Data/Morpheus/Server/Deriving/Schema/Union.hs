{-# LANGUAGE ConstraintKinds #-}
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
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
  )
import Data.Morpheus.Server.Deriving.Utils.Use (UseGQLType (..), useTypename)
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
import Relude

buildUnionTypeContent ::
  (gql a) =>
  UseGQLType gql ->
  CatType kind a ->
  [ConsRep (Maybe (FieldContent TRUE kind CONST))] ->
  SchemaT k (TypeContent TRUE kind CONST)
buildUnionTypeContent gql scope cons = mkUnionType scope unionRef unionCons
  where
    unionRef = fieldTypeName <$> concatMap consFields unionRefRep
    (unionRefRep, unionCons) = partition (isUnionRef (useTypename gql scope)) cons

mkUnionType ::
  CatType kind a ->
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
  CatType kind a ->
  [ConsRep (Maybe (FieldContent TRUE kind CONST))] ->
  SchemaT c [TypeName]
buildUnions proxy cons =
  traverse_ (defineObjectType proxy) cons $> fmap consName cons
