{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Validation.Document.Interface
  ( validateImplements,
  )
where

import Data.Morpheus.Error.Document.Interface
  ( ImplementsError (..),
    partialImplements,
  )
import Data.Morpheus.Internal.Utils
  ( KeyOf (..),
    Selectable (..),
    empty,
    failure,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentDefinition (..),
    ArgumentsDefinition,
    CONST,
    DirectiveLocation (..),
    FieldContent (..),
    FieldDefinition (..),
    FieldsDefinition,
    OUT,
    Subtyping (..),
    TRUE,
    TypeName,
    TypeRef (..),
  )
import Data.Morpheus.Types.Internal.Validation (selectType)
import Data.Morpheus.Types.Internal.Validation.SchemaValidator
  ( Field (..),
    ON_INTERFACE,
    ON_TYPE,
    SchemaValidator,
    TypeEntity (..),
    TypeSystemContext (..),
    constraintInterface,
    inArgument,
    inField,
    inInterface,
  )
import Data.Morpheus.Validation.Internal.Directive (validateDirectives)
import Relude hiding (empty, local)

validateImplements ::
  [TypeName] ->
  FieldsDefinition OUT CONST ->
  SchemaValidator (TypeEntity ON_TYPE) [TypeName]
validateImplements objectImplements objectFields =
  ( traverse selectInterface objectImplements
      >>= traverse_ (mustBeSubset objectFields)
  )
    $> objectImplements

-------------------------------
selectInterface ::
  TypeName ->
  SchemaValidator ctx (TypeName, FieldsDefinition OUT CONST)
selectInterface = selectType >=> constraintInterface

mustBeSubset ::
  FieldsDefinition OUT CONST ->
  (TypeName, FieldsDefinition OUT CONST) ->
  SchemaValidator (TypeEntity ON_TYPE) ()
mustBeSubset objFields (typeName, fields) =
  inInterface typeName $
    traverse_ (checkInterfaceField objFields) fields

checkInterfaceField ::
  FieldsDefinition OUT CONST ->
  FieldDefinition OUT CONST ->
  SchemaValidator (TypeEntity ON_INTERFACE) ()
checkInterfaceField
  objFields
  interfaceField@FieldDefinition
    { fieldName,
      fieldDirectives
    } =
    inField fieldName $
      validateDirectives FIELD_DEFINITION fieldDirectives
        *> selectOr err (`isCompatibleTo` interfaceField) fieldName objFields
    where
      err = failImplements Missing

class StructuralCompatibility a where
  isCompatibleTo :: a -> a -> SchemaValidator (Field ON_INTERFACE) ()

isCompatibleBy :: StructuralCompatibility a => (t -> a) -> t -> t -> SchemaValidator (Field ON_INTERFACE) ()
isCompatibleBy f a b = f a `isCompatibleTo` f b

instance StructuralCompatibility (FieldDefinition OUT CONST) where
  f1 `isCompatibleTo` f2 =
    isCompatibleBy fieldType f1 f2
      *> isCompatibleBy (fieldArgs . fieldContent) f1 f2

fieldArgs :: Maybe (FieldContent TRUE OUT s) -> ArgumentsDefinition s
fieldArgs (Just (FieldArgs args)) = args
fieldArgs _ = empty

instance StructuralCompatibility (ArgumentsDefinition s) where
  subArguments `isCompatibleTo` arguments = traverse_ hasCompatibleSubArgument arguments
    where
      -- hasCompatibleSubArgument :: ArgumentDefinition s -> SchemaValidator Field ()
      hasCompatibleSubArgument argument =
        inArgument (keyOf argument) $
          selectOr (failImplements Missing) (`isCompatibleTo` argument) (keyOf argument) subArguments

instance StructuralCompatibility (ArgumentDefinition s) where
  isCompatibleTo = isCompatibleBy (fieldType . argument)

instance StructuralCompatibility TypeRef where
  t1 `isCompatibleTo` t2
    | t1 `isSubtype` t2 = pure ()
    | otherwise = failImplements UnexpectedType {expectedType = t2, foundType = t1}

failImplements ::
  ImplementsError ->
  SchemaValidator (Field ON_INTERFACE) a
failImplements err = do
  x <- asks local
  failure $ partialImplements x err
