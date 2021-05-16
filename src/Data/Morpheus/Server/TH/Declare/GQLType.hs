{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Declare.GQLType
  ( deriveGQLType,
  )
where

--
-- MORPHEUS
import Data.Morpheus.Internal.TH
  ( apply,
    applyVars,
    typeInstanceDec,
  )
import Data.Morpheus.Internal.Utils
  ( stripConstructorNamespace,
    stripFieldNamespace,
  )
import Data.Morpheus.Server.Internal.TH.Types
  ( ServerDecContext (..),
    ServerTypeDefinition (..),
  )
import Data.Morpheus.Server.Internal.TH.Utils
  ( funDProxy,
    kindName,
    mkTypeableConstraints,
    tyConArgs,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    GQLTypeOptions (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( TypeKind (..),
    TypeName (..),
  )
import Language.Haskell.TH
import Relude

dropNamespaceOptions :: TypeKind -> TypeName -> GQLTypeOptions -> GQLTypeOptions
dropNamespaceOptions KindEnum tName opt = opt {constructorTagModifier = stripConstructorNamespace tName}
dropNamespaceOptions _ tName opt = opt {fieldLabelModifier = stripFieldNamespace tName}

deriveGQLType :: ServerDecContext -> ServerTypeDefinition cat s -> Q [Dec]
deriveGQLType _ ServerInterfaceDefinition {} = pure []
deriveGQLType
  ServerDecContext {namespace}
  ServerTypeDefinition
    { tName,
      tKind,
      gqlTypeDescription,
      gqlTypeDescriptions,
      gqlTypeDirectives,
      gqlTypeFieldContents
    } =
    pure <$> instanceD constrains headType (typeFamilies : functions)
    where
      functions =
        funDProxy
          [ ('description, [|gqlTypeDescription|]),
            ('typeOptions, typeOptionsFunc),
            ('getDescriptions, [|gqlTypeDescriptions|]),
            ('getDirectives, [|gqlTypeDirectives|]),
            ('getFieldContents, [|gqlTypeFieldContents|])
          ]
        where
          typeOptionsFunc
            | namespace = [|dropNamespaceOptions tKind tName|]
            | otherwise = [|id|]
      --------------------------------
      typeArgs = tyConArgs tKind
      --------------------------------
      headType = apply ''GQLType [applyVars tName typeArgs]
      ---------------------------------------------------
      constrains = mkTypeableConstraints typeArgs
      -------------------------------------------------
      typeFamilies = do
        currentType <- applyVars tName typeArgs
        pure $ typeInstanceDec ''KIND currentType (ConT (kindName tKind))
