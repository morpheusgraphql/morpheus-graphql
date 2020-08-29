{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Fields
  ( Arguments,
    Argument (..),
    ArgumentDefinition,
    ArgumentsDefinition (..),
    FieldDefinition (..),
    FieldsDefinition,
    Fields (..),
    FieldContent (..),
    InputFieldsDefinition,
    DirectiveDefinitions,
    DirectiveDefinition (..),
    Directives,
    Directive (..),
    fieldVisibility,
    toListField,
    lookupDeprecated,
    lookupDeprecatedReason,
    unsafeFromFields,
    fieldsToArguments,
    fieldContentArgs,
    mkInputValue,
    mkObjectField,
  )
where

-- MORPHEUS

import Data.Foldable (Foldable)
import Data.Functor ((<$>), Functor (..))
import Data.List (find)
import Data.Maybe (Maybe (..))
import Data.Morpheus.Error.NameCollision
  ( NameCollision (..),
  )
import Data.Morpheus.Internal.Utils
  ( Collection (..),
    KeyOf (..),
    Listable (..),
    Merge (..),
    Namespace (..),
    Selectable (..),
    elems,
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    renderArguments,
    renderEntry,
    renderObject,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( Description,
    FieldName,
    FieldName (..),
    Msg (..),
    Nullable (..),
    Position,
    TRUE,
    TypeName,
    TypeRef (..),
    TypeWrapper (..),
    ValidationError (..),
    msgValidation,
    sysFields,
  )
import Data.Morpheus.Types.Internal.AST.DirectiveLocation (DirectiveLocation)
import Data.Morpheus.Types.Internal.AST.OrdMap
  ( OrdMap,
    unsafeFromValues,
  )
import Data.Morpheus.Types.Internal.AST.Stage
  ( Stage,
  )
import Data.Morpheus.Types.Internal.AST.TypeCategory
  ( ANY,
    ELEM,
    IN,
    OUT,
    ToCategory (..),
    TypeCategory,
    toAny,
  )
import Data.Morpheus.Types.Internal.AST.Value
  ( ScalarValue (..),
    Value (..),
  )
import Data.Semigroup (Semigroup ((<>)))
import Data.Traversable (Traversable)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift (..))
import Prelude
  ( ($),
    (.),
    Bool (..),
    Eq,
    Show,
    filter,
    notElem,
  )

-- scalar
------------------------------------------------------------------
data Argument (valid :: Stage) = Argument
  { argumentName :: FieldName,
    argumentPosition :: Position,
    argumentValue :: Value valid
  }
  deriving (Show, Eq, Lift)

instance KeyOf FieldName (Argument stage) where
  keyOf = argumentName

instance RenderGQL (Argument s) where
  render Argument {argumentName, argumentValue} =
    renderEntry argumentName argumentValue

instance NameCollision (Argument s) where
  nameCollision Argument {argumentName, argumentPosition} =
    ValidationError
      { validationMessage = "There can Be only One Argument Named " <> msg argumentName,
        validationLocations = [argumentPosition]
      }

type Arguments (s :: Stage) = OrdMap FieldName (Argument s)

-- directive
------------------------------------------------------------------
data Directive (s :: Stage) = Directive
  { directiveName :: FieldName,
    directivePosition :: Position,
    directiveArgs :: Arguments s
  }
  deriving (Show, Lift, Eq)

instance KeyOf FieldName (Directive s) where
  keyOf = directiveName

type Directives s = [Directive s]

data DirectiveDefinition s = DirectiveDefinition
  { directiveDefinitionName :: FieldName,
    directiveDefinitionDescription :: Maybe Description,
    directiveDefinitionLocations :: [DirectiveLocation],
    directiveDefinitionArgs :: ArgumentsDefinition s
  }
  deriving (Show, Lift)

type DirectiveDefinitions s = [DirectiveDefinition s]

instance KeyOf FieldName (DirectiveDefinition s) where
  keyOf = directiveDefinitionName

instance Selectable FieldName (ArgumentDefinition s) (DirectiveDefinition s) where
  selectOr fb f key DirectiveDefinition {directiveDefinitionArgs} =
    selectOr fb f key directiveDefinitionArgs

lookupDeprecated :: [Directive s] -> Maybe (Directive s)
lookupDeprecated = find isDeprecation
  where
    isDeprecation Directive {directiveName = "deprecated"} = True
    isDeprecation _ = False

lookupDeprecatedReason :: Directive s -> Maybe Description
lookupDeprecatedReason Directive {directiveArgs} =
  selectOr
    Nothing
    argumentStringValue
    ("reason" :: FieldName)
    directiveArgs

argumentStringValue :: Argument s -> Maybe Description
argumentStringValue Argument {argumentValue = Null} = Nothing
argumentStringValue Argument {argumentValue = (Scalar (String x))} = Just x
argumentStringValue _ = Just "can't read deprecated Reason Value"

instance ToCategory FieldDefinition a ANY where
  toCategory FieldDefinition {fieldContent, ..} = FieldDefinition {fieldContent = toAny <$> fieldContent, ..}

instance ToCategory (FieldContent TRUE) a ANY where
  toCategory (FieldArgs x) = FieldArgs x
  toCategory (DefaultInputValue x) = DefaultInputValue x

newtype Fields def = Fields
  {unFields :: OrdMap FieldName def}
  deriving
    ( Show,
      Lift,
      Functor,
      Foldable,
      Traversable
    )

deriving instance (KeyOf FieldName def) => Collection def (Fields def)

instance Merge (FieldsDefinition cat s) where
  merge path (Fields x) (Fields y) = Fields <$> merge path x y

instance Selectable FieldName (FieldDefinition cat s) (Fields (FieldDefinition cat s)) where
  selectOr fb f name (Fields lib) = selectOr fb f name lib

unsafeFromFields :: [FieldDefinition cat s] -> FieldsDefinition cat s
unsafeFromFields = Fields . unsafeFromValues

fieldsToArguments :: FieldsDefinition IN s -> ArgumentsDefinition s
fieldsToArguments = ArgumentsDefinition Nothing . unFields

instance (KeyOf FieldName def, NameCollision def) => Listable def (Fields def) where
  fromElems = fmap Fields . fromElems
  elems = elems . unFields

-- 3.6 Objects : https://graphql.github.io/graphql-spec/June2018/#sec-Objects
------------------------------------------------------------------------------
--  ObjectTypeDefinition:
--    Description(opt) type Name ImplementsInterfaces(opt) Directives(Const)(opt) FieldsDefinition(opt)
--
--  ImplementsInterfaces
--    implements &(opt) NamedType
--    ImplementsInterfaces & NamedType
--
--  FieldsDefinition
--    { FieldDefinition(list) }
--
type FieldsDefinition cat s = Fields (FieldDefinition cat s)

--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
--
-- InputValueDefinition
--   Description(opt) Name: Type DefaultValue(opt) Directives[Const](opt)

data FieldDefinition (cat :: TypeCategory) (s :: Stage) = FieldDefinition
  { fieldName :: FieldName,
    fieldDescription :: Maybe Description,
    fieldType :: TypeRef,
    fieldDirectives :: [Directive s],
    fieldContent :: Maybe (FieldContent TRUE cat s)
  }
  deriving (Show, Lift, Eq)

instance Namespace (FieldDefinition c s) where
  stripNamespace ns f = f {fieldName = stripNamespace ns (fieldName f)}

data FieldContent (bool :: Bool) (cat :: TypeCategory) (s :: Stage) where
  DefaultInputValue ::
    { defaultInputValue :: Value s
    } ->
    FieldContent (ELEM IN cat) cat s
  FieldArgs ::
    { fieldArgsDef :: ArgumentsDefinition s
    } ->
    FieldContent (ELEM OUT cat) cat s

fieldContentArgs :: FieldContent b cat s -> ArgumentsDefinition s
fieldContentArgs (FieldArgs args) = args
fieldContentArgs _ = empty

deriving instance Eq (FieldContent bool cat s)

deriving instance Show (FieldContent bool cat s)

deriving instance Lift (FieldContent bool cat s)

instance KeyOf FieldName (FieldDefinition cat s) where
  keyOf = fieldName

instance Selectable FieldName (ArgumentDefinition s) (FieldDefinition OUT s) where
  selectOr fb f key FieldDefinition {fieldContent = Just (FieldArgs args)} = selectOr fb f key args
  selectOr fb _ _ _ = fb

instance NameCollision (FieldDefinition cat s) where
  nameCollision FieldDefinition {fieldName} =
    "There can Be only One field Named " <> msgValidation fieldName

instance RenderGQL (FieldDefinition cat s) where
  render FieldDefinition {fieldName = FieldName name, fieldType, fieldContent = Just (FieldArgs args)} =
    name <> render args <> ": " <> render fieldType
  render FieldDefinition {fieldName, fieldType} =
    renderEntry fieldName fieldType

instance RenderGQL (FieldsDefinition cat s) where
  render = renderObject . filter fieldVisibility . elems

instance Nullable (FieldDefinition cat s) where
  isNullable = isNullable . fieldType
  toNullable field = field {fieldType = toNullable (fieldType field)}

fieldVisibility :: FieldDefinition cat s -> Bool
fieldVisibility FieldDefinition {fieldName} = fieldName `notElem` sysFields

createField ::
  Maybe (FieldContent TRUE cat s) ->
  FieldName ->
  [TypeWrapper] ->
  TypeName ->
  FieldDefinition cat s
createField fieldContent fieldName typeWrappers typeConName =
  FieldDefinition
    { fieldName,
      fieldContent,
      fieldDescription = Nothing,
      fieldType = TypeRef {typeConName, typeWrappers, typeArgs = Nothing},
      fieldDirectives = []
    }

mkInputValue :: FieldName -> [TypeWrapper] -> TypeName -> FieldDefinition cat s
mkInputValue = createField Nothing

mkObjectField ::
  ArgumentsDefinition s ->
  FieldName ->
  [TypeWrapper] ->
  TypeName ->
  FieldDefinition OUT s
mkObjectField args = createField (Just $ FieldArgs args)

toListField :: FieldDefinition cat s -> FieldDefinition cat s
toListField dataField = dataField {fieldType = listW (fieldType dataField)}
  where
    listW alias@TypeRef {typeWrappers} =
      alias {typeWrappers = TypeList : typeWrappers}

-- 3.10 Input Objects: https://spec.graphql.org/June2018/#sec-Input-Objects
---------------------------------------------------------------------------
--- InputFieldsDefinition
-- { InputValueDefinition(list) }

type InputFieldsDefinition s = Fields (InputValueDefinition s)

type InputValueDefinition = FieldDefinition IN

-- 3.6.1 Field Arguments : https://graphql.github.io/graphql-spec/June2018/#sec-Field-Arguments
-----------------------------------------------------------------------------------------------
-- ArgumentsDefinition:
--   (InputValueDefinition(list))

data ArgumentsDefinition s = ArgumentsDefinition
  { argumentsTypename :: Maybe TypeName,
    arguments :: OrdMap FieldName (ArgumentDefinition s)
  }
  deriving (Show, Lift, Eq)

instance RenderGQL (ArgumentsDefinition s) where
  render ArgumentsDefinition {arguments} = renderArguments (elems arguments)

type ArgumentDefinition = FieldDefinition IN

instance Selectable FieldName (ArgumentDefinition s) (ArgumentsDefinition s) where
  selectOr fb f key (ArgumentsDefinition _ args) = selectOr fb f key args

instance Collection (ArgumentDefinition s) (ArgumentsDefinition s) where
  empty = ArgumentsDefinition Nothing empty
  singleton = ArgumentsDefinition Nothing . singleton

instance Listable (ArgumentDefinition s) (ArgumentsDefinition s) where
  elems (ArgumentsDefinition _ args) = elems args
  fromElems args = ArgumentsDefinition Nothing <$> fromElems args
