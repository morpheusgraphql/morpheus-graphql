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
    Name (unpackName),
    Position,
    Ref (..),
    TypeDefinition (..),
    TypeKind,
    TypeName,
    TypeWrapper,
    kindOf,
  )
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
    path :: [Text]
  }
  deriving (Show)

updateSelectionName :: FieldName -> Scope -> Scope
updateSelectionName name Scope {..} =
  Scope
    { fieldName = name,
      path = path <> [unpackName name],
      ..
    }

setSelection :: TypeDefinition a s -> Ref FieldName -> Scope -> Scope
setSelection currentType Ref {refName, refPosition} =
  updateSelectionName refName . update
  where
    update Scope {..} =
      Scope
        { currentTypeName = typeName currentType,
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
setDirective
  Directive
    { directiveName,
      directivePosition
    } = updateSelectionName directiveName . update
    where
      update Scope {..} =
        Scope
          { position = Just directivePosition,
            kind = DIRECTIVE,
            ..
          }

setType ::
  (TypeDefinition cat s, TypeWrapper) ->
  Scope ->
  Scope
setType (t@TypeDefinition {typeName}, wrappers) Scope {..} =
  Scope
    { currentTypeName = typeName,
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

renderSection :: RenderGQL a => GQLError -> a -> GQLError
renderSection label content =
  "\n\n" <> label <> ":\n" <> line
    <> "\n\n"
    <> msg (render content)
    <> "\n\n"
  where
    line = stimes (50 :: Int) "-"
