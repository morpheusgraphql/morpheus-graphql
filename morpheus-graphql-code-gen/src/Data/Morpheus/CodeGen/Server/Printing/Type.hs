{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Printing.Type
  ( renderTypes,
  )
where

import Data.Morpheus.CodeGen.Server.Internal.AST
  ( FIELD_TYPE_WRAPPER (..),
    ServerConstructorDefinition (..),
    ServerFieldDefinition (..),
    ServerTypeDefinition (..),
    TypeKind (..),
    unpackName,
  )
import Data.Morpheus.CodeGen.Server.Printing.GQLType
  ( renderGQLType,
  )
import Data.Morpheus.CodeGen.Utils
  ( TypeDoc (TypeDoc, unDoc),
    appendType,
    label,
    parametrizedType,
    renderDeriving,
    renderName,
    renderType,
    renderTypeRef,
    renderWrapped,
  )
import Prettyprinter
  ( Doc,
    comma,
    enclose,
    indent,
    line,
    nest,
    pretty,
    punctuate,
    vsep,
    (<+>),
  )
import Relude hiding (show)
import Prelude (show)

type Result = Either Text

renderTypes :: [ServerTypeDefinition] -> Either Text (Doc ann)
renderTypes = fmap vsep . traverse render

class RenderType a where
  render :: a -> Result (Doc ann)

instance RenderType ServerTypeDefinition where
  render ServerInterfaceDefinition {} = fail "not supported"
  -- TODO: on scalar we should render user provided type
  render ServerTypeDefinition {tKind = KindScalar, tName} =
    pure $ label tName <> "type" <+> pretty tName <+> "= Int"
  render typeDef@ServerTypeDefinition {tName, tCons, typeParameters, derives} = do
    typeRendering <- renderTypeDef
    pure $ label tName <> vsep [typeRendering, renderGQLType typeDef]
    where
      renderTypeDef = do
        cons <- renderConstructors tCons
        let derivations = renderDeriving derives
        pure $
          "data"
            <+> parametrizedType tName typeParameters
              <> cons
              <> line
              <> indent 2 derivations
              <> line
      renderConstructors [cons] = (" =" <+>) <$> render cons
      renderConstructors conses = nest 2 . (line <>) . vsep . prefixVariants <$> traverse render conses
      prefixVariants (x : xs) = "=" <+> x : map ("|" <+>) xs
      prefixVariants [] = []
  render typeDef@DirectiveTypeDefinition {directiveConstructor, directiveDerives} = do
    typeRendering <- renderTypeDef
    pure $ label name <> vsep [typeRendering, renderGQLType typeDef]
    where
      renderTypeDef = do
        cons <- (" =" <+>) <$> render directiveConstructor
        let derivations = renderDeriving directiveDerives
        pure $
          "data"
            <+> parametrizedType name []
              <> cons
              <> line
              <> indent 2 derivations
              <> line
      name = unpackName (constructorName directiveConstructor)

instance RenderType ServerConstructorDefinition where
  render ServerConstructorDefinition {constructorName, constructorFields = []} =
    pure $ renderName constructorName
  render ServerConstructorDefinition {constructorName, constructorFields} = do
    fields <- traverse render constructorFields
    pure $ renderName constructorName <> renderSet fields
    where
      renderSet = nest 2 . enclose "\n{ " "\n}" . nest 2 . vsep . punctuate comma

instance RenderType ServerFieldDefinition where
  render
    ServerFieldDefinition
      { fieldName,
        wrappers,
        fieldType
      } =
      pure $
        pretty (unpackName fieldName :: Text)
          <+> "::"
          <+> unDoc (foldr renderWrapper (TypeDoc False $ pretty fieldType) wrappers)

renderWrapper :: FIELD_TYPE_WRAPPER -> TypeDoc n -> TypeDoc n
renderWrapper PARAMETRIZED = \x -> TypeDoc True (unDoc x <+> "m")
renderWrapper MONAD = appendType "m"
renderWrapper SUBSCRIPTION = id
renderWrapper (GQL_WRAPPER typeWrappers) = renderWrapped typeWrappers
renderWrapper (ARG name) = TypeDoc True . ((renderName name <+> "->") <+>) . unDoc
renderWrapper (TAGGED_ARG name typeRef) =
  TypeDoc True
    . ( ( "Arg"
            <+> pretty (show name)
            <+> renderType (renderTypeRef typeRef)
            <+> "->"
        )
          <+>
      )
    . unDoc
