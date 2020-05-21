{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Schema.Directives (defaultDirectives) where

import Data.Morpheus.Internal.Utils
  ( singleton,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    DirectiveDefinition (..),
    DirectiveLocation (..),
    TypeWrapper (..),
    createField,
  )

defaultDirectives :: [DirectiveDefinition]
defaultDirectives =
  [ DirectiveDefinition
      { directiveDefinitionName = "skip",
        directiveDefinitionDescription = Just "Directs the executor to skip this field or fragment when the `if` argument is true.",
        directiveDefinitionLocations = [FIELD, FRAGMENT_SPREAD, INLINE_FRAGMENT],
        directiveDefinitionArgs = argumentsIf
      },
    DirectiveDefinition
      { directiveDefinitionName = "include",
        directiveDefinitionDescription = Just "Directs the executor to include this field or fragment only when the `if` argument is true.",
        directiveDefinitionLocations = [FIELD, FRAGMENT_SPREAD, INLINE_FRAGMENT],
        directiveDefinitionArgs = argumentsIf
      },
    DirectiveDefinition
      { directiveDefinitionName = "deprecated",
        directiveDefinitionDescription = Just "Marks an element of a GraphQL schema as no longer supported.",
        directiveDefinitionLocations = [FIELD_DEFINITION, ENUM_VALUE],
        directiveDefinitionArgs =
          singleton $
            createField
              NoArguments
              "reason"
              ([TypeMaybe], "String")
      }
  ]

argumentsIf :: ArgumentsDefinition
argumentsIf = singleton $ createField NoArguments "if" ([], "Boolean")
