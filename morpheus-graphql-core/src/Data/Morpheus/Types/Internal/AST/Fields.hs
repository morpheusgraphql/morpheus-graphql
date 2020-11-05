{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Fields
  ( Arguments,
    Argument (..),
    ArgumentDefinition,
    ArgumentsDefinition (..),
    FieldDefinition (..),
    FieldsDefinition,
    FieldContent (..),
    InputFieldsDefinition,
    DirectiveDefinitions,
    DirectiveDefinition (..),
    Directives,
    Directive (..),
    fieldVisibility,
    lookupDeprecated,
    lookupDeprecatedReason,
    unsafeFromFields,
    fieldsToArguments,
    fieldContentArgs,
    mkInputValue,
    mkObjectField,
    mkField,
    renderArgumentValues,
    renderDirectives,
  )
where

import Data.Morpheus.Error.NameCollision
  ( NameCollision (..),
  )
import Data.Morpheus.Ext.OrdMap
  ( OrdMap,
    unsafeFromList,
  )
import Data.Morpheus.Internal.Utils
  ( Collection (..),
    Elems (..),
    Failure,
    FromElems (..),
    KeyOf (..),
    Selectable (..),
    toPair,
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    Rendering,
    intercalate,
    renderArguments,
    renderEntry,
    renderObject,
    space,
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
    ValidationErrors,
    msgValidation,
    sysFields,
  )
import Data.Morpheus.Types.Internal.AST.DirectiveLocation (DirectiveLocation)
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
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift (..))
import Relude hiding (empty, intercalate)

-- scalar
------------------------------------------------------------------
data Argument (valid :: Stage) = Argument
  { argumentPosition :: Position,
    argumentName :: FieldName,
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

renderArgumentValues :: Arguments s -> Rendering
renderArgumentValues = renderArguments . filter notNull . elems
  where
    notNull Argument {argumentValue = Null} = False
    notNull _ = True

-- directive
------------------------------------------------------------------
data Directive (s :: Stage) = Directive
  { directivePosition :: Position,
    directiveName :: FieldName,
    directiveArgs :: Arguments s
  }
  deriving (Show, Lift, Eq)

instance KeyOf FieldName (Directive s) where
  keyOf = directiveName

instance RenderGQL (Directive s) where
  render Directive {..} =
    "@" <> render directiveName
      <> renderArgumentValues directiveArgs

type Directives s = [Directive s]

renderDirectives :: Directives s -> Rendering
renderDirectives xs
  | null dirs = ""
  | otherwise = space <> intercalate space (fmap render dirs)
  where
    dirs = filter notSystem xs
    notSystem Directive {directiveName = "include"} = False
    notSystem Directive {directiveName = "skip"} = False
    notSystem _ = True

data DirectiveDefinition s = DirectiveDefinition
  { directiveDefinitionName :: FieldName,
    directiveDefinitionDescription :: Maybe Description,
    directiveDefinitionArgs :: ArgumentsDefinition s,
    directiveDefinitionLocations :: [DirectiveLocation]
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

unsafeFromFields :: [FieldDefinition cat s] -> FieldsDefinition cat s
unsafeFromFields = unsafeFromList . fmap toPair

fieldsToArguments :: FieldsDefinition IN s -> ArgumentsDefinition s
fieldsToArguments = ArgumentsDefinition Nothing

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
type FieldsDefinition cat s = OrdMap FieldName (FieldDefinition cat s)

--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
--
-- InputValueDefinition
--   Description(opt) Name: Type DefaultValue(opt) Directives[Const](opt)

data FieldDefinition (cat :: TypeCategory) (s :: Stage) = FieldDefinition
  { fieldDescription :: Maybe Description,
    fieldName :: FieldName,
    fieldType :: TypeRef,
    fieldContent :: Maybe (FieldContent TRUE cat s),
    fieldDirectives :: [Directive s]
  }
  deriving (Show, Lift, Eq)

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
  render FieldDefinition {fieldName, fieldType, fieldContent = Just (FieldArgs args)} =
    render fieldName <> render args <> ": " <> render fieldType
  render FieldDefinition {fieldName, fieldType} =
    renderEntry fieldName fieldType

instance RenderGQL (FieldsDefinition cat s) where
  render = renderObject . filter fieldVisibility . elems

instance Nullable (FieldDefinition cat s) where
  isNullable = isNullable . fieldType
  toNullable field = field {fieldType = toNullable (fieldType field)}

fieldVisibility :: FieldDefinition cat s -> Bool
fieldVisibility FieldDefinition {fieldName} = fieldName `notElem` sysFields

mkField ::
  Maybe (FieldContent TRUE cat s) ->
  FieldName ->
  TypeRef ->
  FieldDefinition cat s
mkField fieldContent fieldName fieldType =
  FieldDefinition
    { fieldName,
      fieldContent,
      fieldDescription = Nothing,
      fieldType,
      fieldDirectives = []
    }

mkInputValue :: FieldName -> [TypeWrapper] -> TypeName -> FieldDefinition cat s
mkInputValue fieldName typeWrappers typeConName =
  mkField
    Nothing
    fieldName
    TypeRef {typeWrappers, typeConName, typeArgs = Nothing}

mkObjectField ::
  ArgumentsDefinition s ->
  FieldName ->
  [TypeWrapper] ->
  TypeName ->
  FieldDefinition OUT s
mkObjectField args fieldName typeWrappers typeConName =
  mkField
    (Just $ FieldArgs args)
    fieldName
    TypeRef {typeWrappers, typeConName, typeArgs = Nothing}

-- 3.10 Input Objects: https://spec.graphql.org/June2018/#sec-Input-Objects
---------------------------------------------------------------------------
--- InputFieldsDefinition
-- { InputValueDefinition(list) }

type InputFieldsDefinition s = OrdMap FieldName (InputValueDefinition s)

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

instance (Monad m, Failure ValidationErrors m) => FromElems m (ArgumentDefinition s) (ArgumentsDefinition s) where
  fromElems args = ArgumentsDefinition Nothing <$> fromElems args

instance Elems (ArgumentDefinition s) (ArgumentsDefinition s) where
  elems (ArgumentsDefinition _ args) = elems args
