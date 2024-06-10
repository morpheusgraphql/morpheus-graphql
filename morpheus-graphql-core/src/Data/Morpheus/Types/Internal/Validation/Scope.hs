{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Validation.Scope
  ( Scope (..),
    ScopeKind (..),
    renderScope,
    renderSection,
    setPosition,
    setSelection,
    setDirective,
    setType,
  )
where

import Data.Morpheus.Rendering.RenderGQL (RenderGQL, render)
import Data.Morpheus.Types.Internal.AST
  ( Directive (..),
    FieldName,
    GQLError,
    Msg (msg),
    Position,
    Ref (..),
    TypeDefinition (..),
    TypeKind,
    TypeName,
    TypeWrapper,
    kindOf,
  )
import Data.Morpheus.Types.Internal.AST.Error (PropName)
import Relude

data ScopeKind
  = DIRECTIVE
  | SELECTION
  | TYPE
  deriving (Show)

data Scope = Scope
  { position :: Maybe Position,
    currentTypeName :: TypeName,
    currentTypeKind :: TypeKind,
    currentTypeWrappers :: TypeWrapper,
    fieldName :: FieldName,
    kind :: ScopeKind,
    path :: [PropName]
  }
  deriving (Show)

setSelection :: TypeDefinition a s -> Ref FieldName -> Scope -> Scope
setSelection currentType Ref {refName, refPosition} Scope {..} =
  Scope
    { fieldName = refName,
      -- path = path <> [unpackName refName],
      currentTypeName = typeName currentType,
      currentTypeKind = kindOf currentType,
      position = Just refPosition,
      ..
    }

setPosition ::
  Position ->
  Scope ->
  Scope
setPosition pos Scope {..} = Scope {position = Just pos, ..}

setDirective :: Directive s -> Scope -> Scope
setDirective Directive {..} Scope {..} =
  Scope
    { fieldName = directiveName,
      position = Just directivePosition,
      kind = DIRECTIVE,
      ..
    }

setType :: TypeDefinition c s -> TypeWrapper -> Scope -> Scope
setType t wrappers Scope {..} =
  Scope
    { currentTypeName = typeName t,
      currentTypeKind = kindOf t,
      currentTypeWrappers = wrappers,
      ..
    }

renderScope :: Scope -> GQLError
renderScope
  Scope
    { currentTypeName,
      currentTypeKind,
      fieldName
    } =
    renderSection
      "Scope"
      ( "referenced by type "
          <> render currentTypeName
          <> " of kind "
          <> render currentTypeKind
          <> " in field "
          <> render fieldName
      )

renderSection :: (RenderGQL a) => GQLError -> a -> GQLError
renderSection label content =
  "\n\n"
    <> label
    <> ":\n"
    <> line
    <> "\n\n"
    <> msg (render content)
    <> "\n\n"
  where
    line = stimes (50 :: Int) "-"
