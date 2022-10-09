{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Printing.Type
  ( renderTypes,
  )
where

import Data.Morpheus.CodeGen.Printer
  ( Printer (..),
    parametrizedType,
    print,
    print',
    renderDeriving,
    unpack,
  )
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( ServerConstructorDefinition (..),
    ServerTypeDefinition (..),
    TypeKind (..),
    unpackName,
  )
import Data.Morpheus.CodeGen.Server.Printing.GQLType
  ( renderGQLType,
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
import Relude hiding (print, show)

type Result = Either Text

renderTypes :: [ServerTypeDefinition] -> Either Text (Doc ann)
renderTypes = fmap vsep . traverse render

class RenderType a where
  render :: a -> Result (Doc ann)

instance RenderType ServerTypeDefinition where
  render ServerInterfaceDefinition {} = fail "not supported"
  -- TODO: on scalar we should render user provided type
  render ServerTypeDefinition {tKind = KindScalar, tName} =
    pure $ "type" <+> pretty tName <+> "= Int"
  render typeDef@ServerTypeDefinition {tName, tCons, typeParameters, derives} = do
    typeRendering <- renderTypeDef
    pure $ vsep [typeRendering, renderGQLType typeDef]
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
    pure $ vsep [typeRendering, renderGQLType typeDef]
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
    pure $ print' constructorName
  render ServerConstructorDefinition {constructorName, constructorFields} = do
    let fields = map (unpack . print) constructorFields
    pure $ print' constructorName <> renderSet fields
    where
      renderSet = nest 2 . enclose "\n{ " "\n}" . nest 2 . vsep . punctuate comma
