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
  ( Printer (print),
    ignore,
  )
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( ServerDeclaration (..),
  )
import Data.Morpheus.CodeGen.Server.Printing.GQLType
  ( renderGQLType,
  )
import Prettyprinter
  ( Doc,
    Pretty (..),
    vsep,
    (<+>),
  )
import Relude hiding (print, show)

type Result = Either Text

renderTypes :: [ServerDeclaration] -> Either Text (Doc ann)
renderTypes = fmap vsep . traverse render

class RenderType a where
  render :: a -> Result (Doc ann)

instance RenderType ServerDeclaration where
  render InterfaceType {} = fail "not supported"
  -- TODO: on scalar we should render user provided type
  render ScalarType {scalarTypeName} =
    pure $ "type" <+> ignore (print scalarTypeName) <+> "= Int"
  render (DataType cgType) = pure (pretty cgType)
  render (GQLTypeInstance gqlType) = pure $ renderGQLType gqlType
  render (GQLDirectiveInstance _) = fail "not supported"