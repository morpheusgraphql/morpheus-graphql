{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Directive.FieldVisitor
  ( api,
  )
where

import Data.Kind (Type)
import Data.Morpheus.Server (interpreter)
import Data.Morpheus.Server.Types
  ( Arg (..),
    Describe (..),
    GQLRequest,
    GQLResponse,
    GQLType (..),
    InputTypeNamespace (..),
    Rename (..),
    RootResolver (..),
    Undefined,
    defaultRootResolver,
    fieldDirective',
    typeDirective,
  )
import Data.Text (Text, pack)
import GHC.Generics (Generic)

data Deity = Deity
  { __name :: Text,
    __power :: Maybe Text
  }
  deriving (Generic, Show)

instance GQLType Deity where
  directives _ =
    typeDirective (InputTypeNamespace "Input")
      <> fieldDirective' '__name Describe {text = "name of the deity"}
      <> fieldDirective' '__power Describe {text = "extraterrestrial ability"}
      <> fieldDirective' '__name Rename {newName = "name"}
      <> fieldDirective' '__power Rename {newName = "power"}

data Query (m :: Type -> Type) = Query
  { deity :: Deity,
    printDeity :: Arg "deity" Deity -> m Text
  }
  deriving (Generic, GQLType)

root :: RootResolver IO () Query Undefined Undefined
root =
  defaultRootResolver
    { queryResolver =
        Query
          { deity =
              Deity
                { __name = "Morpheus",
                  __power = Just "Shapeshifting"
                },
            printDeity = pure . pack . show . argValue
          }
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter root
