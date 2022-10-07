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
  ( HSDoc,
    Printer (..),
    apply,
    infix',
    printDoc,
    renderDeriving,
    unpackHSDoc,
    wrapped,
    (.<>),
  )
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
  ( label,
    parametrizedType,
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
    pure $ printDoc constructorName
  render ServerConstructorDefinition {constructorName, constructorFields} = do
    fields <- traverse render constructorFields
    pure $ printDoc constructorName <> renderSet fields
    where
      renderSet = nest 2 . enclose "\n{ " "\n}" . nest 2 . vsep . punctuate comma

instance RenderType ServerFieldDefinition where
  render
    ServerFieldDefinition
      { fieldName,
        wrappers,
        fieldType
      } =
      pure $ unpackHSDoc $ infix' (print fieldName) "::" (foldr renderWrapper (print fieldType) wrappers)

renderWrapper :: FIELD_TYPE_WRAPPER -> HSDoc n -> HSDoc n
renderWrapper PARAMETRIZED = (.<> "m")
renderWrapper MONAD = ("m" .<>)
renderWrapper SUBSCRIPTION = id
renderWrapper (GQL_WRAPPER typeWrappers) = wrapped typeWrappers
renderWrapper (ARG name) = infix' (print name) "->"
renderWrapper (TAGGED_ARG name typeRef) = infix' (apply "Arg" [print (show name), print typeRef]) "->"