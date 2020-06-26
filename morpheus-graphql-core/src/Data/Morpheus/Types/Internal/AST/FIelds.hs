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

import Data.List (find)
-- MORPHEUS
import Data.Morpheus.Error.NameCollision
  ( NameCollision (..),
  )
import Data.Morpheus.Internal.Utils
  ( Collection (..),
    KeyOf (..),
    Listable (..),
    Merge (..),
    Selectable (..),
    elems,
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    renderObject,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( Description,
    FieldName,
    FieldName (..),
    GQLError (..),
    Msg (..),
    Nullable (..),
    Position,
    TRUE,
    TypeName,
    TypeRef (..),
    TypeWrapper (..),
    msg,
    sysFields,
  )
import Data.Morpheus.Types.Internal.AST.DirectiveLocation (DirectiveLocation)
import Data.Morpheus.Types.Internal.AST.OrderedMap
  ( OrderedMap,
    unsafeFromValues,
  )
import Data.Morpheus.Types.Internal.AST.Stage
  ( CONST,
    Stage,
    VALID,
  )
import Data.Morpheus.Types.Internal.AST.TypeCategory
import Data.Morpheus.Types.Internal.AST.Value
  ( ScalarValue (..),
    Value (..),
  )
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (intercalate)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift (..))

-- scalar
------------------------------------------------------------------
data Argument (valid :: Stage) = Argument
  { argumentName :: FieldName,
    argumentValue :: Value valid,
    argumentPosition :: Position
  }
  deriving (Show, Eq, Lift)

instance KeyOf (Argument stage) where
  keyOf = argumentName

instance NameCollision (Argument s) where
  nameCollision _ Argument {argumentName, argumentPosition} =
    GQLError
      { message = "There can Be only One Argument Named " <> msg argumentName,
        locations = [argumentPosition]
      }

type Arguments s = OrderedMap FieldName (Argument s)

-- directive
------------------------------------------------------------------
data Directive (s :: Stage) = Directive
  { directiveName :: FieldName,
    directivePosition :: Position,
    directiveArgs :: Arguments s
  }
  deriving (Show, Lift, Eq)

instance KeyOf (Directive s) where
  keyOf = directiveName

type Directives s = [Directive s]

data DirectiveDefinition = DirectiveDefinition
  { directiveDefinitionName :: FieldName,
    directiveDefinitionDescription :: Maybe Description,
    directiveDefinitionLocations :: [DirectiveLocation],
    directiveDefinitionArgs :: ArgumentsDefinition
  }
  deriving (Show, Lift)

type DirectiveDefinitions = [DirectiveDefinition]

instance KeyOf DirectiveDefinition where
  keyOf = directiveDefinitionName

instance Selectable DirectiveDefinition ArgumentDefinition where
  selectOr fb f key DirectiveDefinition {directiveDefinitionArgs} =
    selectOr fb f key directiveDefinitionArgs

lookupDeprecated :: [Directive VALID] -> Maybe (Directive VALID)
lookupDeprecated = find isDeprecation
  where
    isDeprecation Directive {directiveName = "deprecated"} = True
    isDeprecation _ = False

lookupDeprecatedReason :: Directive VALID -> Maybe Description
lookupDeprecatedReason Directive {directiveArgs} =
  selectOr Nothing (Just . maybeString) "reason" directiveArgs
  where
    maybeString :: Argument VALID -> Description
    maybeString Argument {argumentValue = (Scalar (String x))} = x
    maybeString _ = "can't read deprecated Reason Value"

instance ToAny FieldDefinition where
  toAny FieldDefinition {fieldContent, ..} = FieldDefinition {fieldContent = toAny <$> fieldContent, ..}

instance ToAny (FieldContent TRUE) where
  toAny (FieldArgs x) = FieldArgs x
  toAny (DefaultInputValue x) = DefaultInputValue x

newtype Fields def = Fields
  {unFields :: OrderedMap FieldName def}
  deriving
    ( Show,
      Lift,
      Functor,
      Foldable,
      Traversable
    )

deriving instance (KEY def ~ FieldName, KeyOf def) => Collection def (Fields def)

instance Merge (FieldsDefinition cat) where
  merge path (Fields x) (Fields y) = Fields <$> merge path x y

instance Selectable (Fields (FieldDefinition cat)) (FieldDefinition cat) where
  selectOr fb f name (Fields lib) = selectOr fb f name lib

unsafeFromFields :: [FieldDefinition cat] -> FieldsDefinition cat
unsafeFromFields = Fields . unsafeFromValues

fieldsToArguments :: FieldsDefinition IN -> ArgumentsDefinition
fieldsToArguments = ArgumentsDefinition Nothing . unFields

instance (KEY def ~ FieldName, KeyOf def, NameCollision def) => Listable def (Fields def) where
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
type FieldsDefinition cat = Fields (FieldDefinition cat)

--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
--
-- InputValueDefinition
--   Description(opt) Name: Type DefaultValue(opt) Directives[Const](opt)

data FieldDefinition (cat :: TypeCategory) = FieldDefinition
  { fieldName :: FieldName,
    fieldDescription :: Maybe Description,
    fieldType :: TypeRef,
    fieldContent :: Maybe (FieldContent TRUE cat),
    fieldDirectives :: [Directive VALID]
  }
  deriving (Show, Lift)

data FieldContent (bool :: Bool) (cat :: TypeCategory) where
  DefaultInputValue ::
    { defaultInputValue :: Value CONST
    } ->
    FieldContent (IsSelected cat IN) cat
  FieldArgs ::
    { fieldArgsDef :: ArgumentsDefinition
    } ->
    FieldContent (IsSelected cat OUT) cat

fieldContentArgs :: FieldContent b cat -> ArgumentsDefinition
fieldContentArgs (FieldArgs args) = args
fieldContentArgs _ = empty

deriving instance Show (FieldContent bool cat)

deriving instance Lift (FieldContent bool cat)

instance KeyOf (FieldDefinition cat) where
  keyOf = fieldName

instance Selectable (FieldDefinition OUT) ArgumentDefinition where
  selectOr fb f key FieldDefinition {fieldContent = Just (FieldArgs args)} = selectOr fb f key args
  selectOr fb _ _ _ = fb

instance NameCollision (FieldDefinition cat) where
  nameCollision name _ =
    GQLError
      { message = "There can Be only One field Named " <> msg name,
        locations = []
      }

instance RenderGQL (FieldDefinition cat) where
  render FieldDefinition {fieldName = FieldName name, fieldType, fieldContent = Just (FieldArgs args)} =
    name <> render args <> ": " <> render fieldType
  render FieldDefinition {fieldName = FieldName name, fieldType} =
    name <> ": " <> render fieldType

instance RenderGQL (FieldsDefinition cat) where
  render = renderObject render . filter fieldVisibility . elems

instance Nullable (FieldDefinition cat) where
  isNullable = isNullable . fieldType
  toNullable field = field {fieldType = toNullable (fieldType field)}

fieldVisibility :: FieldDefinition cat -> Bool
fieldVisibility FieldDefinition {fieldName} = fieldName `notElem` sysFields

createField ::
  Maybe (FieldContent TRUE cat) ->
  FieldName ->
  [TypeWrapper] ->
  TypeName ->
  FieldDefinition cat
createField fieldContent fieldName typeWrappers typeConName =
  FieldDefinition
    { fieldName,
      fieldContent,
      fieldDescription = Nothing,
      fieldType = TypeRef {typeConName, typeWrappers, typeArgs = Nothing},
      fieldDirectives = []
    }

mkInputValue :: FieldName -> [TypeWrapper] -> TypeName -> FieldDefinition cat
mkInputValue = createField Nothing

mkObjectField ::
  ArgumentsDefinition ->
  FieldName ->
  [TypeWrapper] ->
  TypeName ->
  FieldDefinition OUT
mkObjectField args = createField (Just $ FieldArgs args)

toListField :: FieldDefinition cat -> FieldDefinition cat
toListField dataField = dataField {fieldType = listW (fieldType dataField)}
  where
    listW alias@TypeRef {typeWrappers} =
      alias {typeWrappers = TypeList : typeWrappers}

-- 3.10 Input Objects: https://spec.graphql.org/June2018/#sec-Input-Objects
---------------------------------------------------------------------------
--- InputFieldsDefinition
-- { InputValueDefinition(list) }

type InputFieldsDefinition = Fields InputValueDefinition

type InputValueDefinition = FieldDefinition IN

-- 3.6.1 Field Arguments : https://graphql.github.io/graphql-spec/June2018/#sec-Field-Arguments
-----------------------------------------------------------------------------------------------
-- ArgumentsDefinition:
--   (InputValueDefinition(list))

data ArgumentsDefinition = ArgumentsDefinition
  { argumentsTypename :: Maybe TypeName,
    arguments :: OrderedMap FieldName ArgumentDefinition
  }
  deriving (Show, Lift)

instance RenderGQL ArgumentsDefinition where
  render ArgumentsDefinition {arguments}
    | null arguments =
      ""
    | otherwise = "(" <> intercalate ", " (map render $ elems arguments) <> ")"

type ArgumentDefinition = FieldDefinition IN

instance Selectable ArgumentsDefinition ArgumentDefinition where
  selectOr fb f key (ArgumentsDefinition _ args) = selectOr fb f key args

instance Collection ArgumentDefinition ArgumentsDefinition where
  empty = ArgumentsDefinition Nothing empty
  singleton = ArgumentsDefinition Nothing . singleton

instance Listable ArgumentDefinition ArgumentsDefinition where
  elems (ArgumentsDefinition _ args) = elems args
  fromElems args = ArgumentsDefinition Nothing <$> fromElems args
