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

import Data.Foldable (Foldable, null)
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
import Data.Morpheus.Types.Internal.AST.OrdMap
  ( OrdMap,
    unsafeFromValues,
  )
import Data.Morpheus.Types.Internal.AST.Stage
  ( Stage,
  )
import Data.Morpheus.Types.Internal.AST.TypeCategory
  ( IN,
    IsSelected,
    OUT,
    ToAny (..),
    TypeCategory,
  )
import Data.Morpheus.Types.Internal.AST.Value
  ( ScalarValue (..),
    Value (..),
  )
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (intercalate)
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
    otherwise,
  )

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

type Arguments (s :: Stage) = OrdMap FieldName (Argument s)

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

data DirectiveDefinition s = DirectiveDefinition
  { directiveDefinitionName :: FieldName,
    directiveDefinitionDescription :: Maybe Description,
    directiveDefinitionLocations :: [DirectiveLocation],
    directiveDefinitionArgs :: ArgumentsDefinition s
  }
  deriving (Show, Lift)

type DirectiveDefinitions s = [DirectiveDefinition s]

instance KeyOf (DirectiveDefinition s) where
  keyOf = directiveDefinitionName

instance Selectable (DirectiveDefinition s) (ArgumentDefinition s) where
  selectOr fb f key DirectiveDefinition {directiveDefinitionArgs} =
    selectOr fb f key directiveDefinitionArgs

lookupDeprecated :: [Directive s] -> Maybe (Directive s)
lookupDeprecated = find isDeprecation
  where
    isDeprecation Directive {directiveName = "deprecated"} = True
    isDeprecation _ = False

lookupDeprecatedReason :: Directive s -> Maybe Description
lookupDeprecatedReason Directive {directiveArgs} =
  selectOr Nothing (Just . maybeString) "reason" directiveArgs
  where
    maybeString :: Argument s -> Description
    maybeString Argument {argumentValue = (Scalar (String x))} = x
    maybeString _ = "can't read deprecated Reason Value"

instance ToAny FieldDefinition where
  toAny FieldDefinition {fieldContent, ..} = FieldDefinition {fieldContent = toAny <$> fieldContent, ..}

instance ToAny (FieldContent TRUE) where
  toAny (FieldArgs x) = FieldArgs x
  toAny (DefaultInputValue x) = DefaultInputValue x

newtype Fields def = Fields
  {unFields :: OrdMap FieldName def}
  deriving
    ( Show,
      Lift,
      Functor,
      Foldable,
      Traversable
    )

deriving instance (KEY def ~ FieldName, KeyOf def) => Collection def (Fields def)

instance Merge (FieldsDefinition cat s) where
  merge path (Fields x) (Fields y) = Fields <$> merge path x y

instance Selectable (Fields (FieldDefinition cat s)) (FieldDefinition cat s) where
  selectOr fb f name (Fields lib) = selectOr fb f name lib

unsafeFromFields :: [FieldDefinition cat s] -> FieldsDefinition cat s
unsafeFromFields = Fields . unsafeFromValues

fieldsToArguments :: FieldsDefinition IN s -> ArgumentsDefinition s
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
    fieldContent :: Maybe (FieldContent TRUE cat s),
    fieldDirectives :: [Directive s]
  }
  deriving (Show, Lift)

data FieldContent (bool :: Bool) (cat :: TypeCategory) (s :: Stage) where
  DefaultInputValue ::
    { defaultInputValue :: Value s
    } ->
    FieldContent (IsSelected cat IN) cat s
  FieldArgs ::
    { fieldArgsDef :: ArgumentsDefinition s
    } ->
    FieldContent (IsSelected cat OUT) cat s

fieldContentArgs :: FieldContent b cat s -> ArgumentsDefinition s
fieldContentArgs (FieldArgs args) = args
fieldContentArgs _ = empty

deriving instance Show (FieldContent bool cat s)

deriving instance Lift (FieldContent bool cat s)

instance KeyOf (FieldDefinition cat s) where
  keyOf = fieldName

instance Selectable (FieldDefinition OUT s) (ArgumentDefinition s) where
  selectOr fb f key FieldDefinition {fieldContent = Just (FieldArgs args)} = selectOr fb f key args
  selectOr fb _ _ _ = fb

instance NameCollision (FieldDefinition cat s) where
  nameCollision name _ =
    GQLError
      { message = "There can Be only One field Named " <> msg name,
        locations = []
      }

instance RenderGQL (FieldDefinition cat s) where
  render FieldDefinition {fieldName = FieldName name, fieldType, fieldContent = Just (FieldArgs args)} =
    name <> render args <> ": " <> render fieldType
  render FieldDefinition {fieldName = FieldName name, fieldType} =
    name <> ": " <> render fieldType

instance RenderGQL (FieldsDefinition cat s) where
  render = renderObject render . filter fieldVisibility . elems

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
  deriving (Show, Lift)

instance RenderGQL (ArgumentsDefinition s) where
  render ArgumentsDefinition {arguments}
    | null arguments =
      ""
    | otherwise = "(" <> intercalate ", " (render <$> elems arguments) <> ")"

type ArgumentDefinition = FieldDefinition IN

instance Selectable (ArgumentsDefinition s) (ArgumentDefinition s) where
  selectOr fb f key (ArgumentsDefinition _ args) = selectOr fb f key args

instance Collection (ArgumentDefinition s) (ArgumentsDefinition s) where
  empty = ArgumentsDefinition Nothing empty
  singleton = ArgumentsDefinition Nothing . singleton

instance Listable (ArgumentDefinition s) (ArgumentsDefinition s) where
  elems (ArgumentsDefinition _ args) = elems args
  fromElems args = ArgumentsDefinition Nothing <$> fromElems args
