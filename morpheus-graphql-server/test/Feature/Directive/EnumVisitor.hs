{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Directive.EnumVisitor
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
    Rename (..),
    RootResolver (..),
    Undefined,
    defaultRootResolver,
    enumDirective',
  )
import Data.Text (Text, pack)
import GHC.Generics (Generic)

data City
  = Athens
  | Sparta
  | CORINTH__UGLY_ENUM_NAME
  | Delphi
  | ARgos
  deriving
    (Generic, Show)

instance GQLType City where
  directives _ =
    enumDirective' 'Sparta Describe {text = "city of warriors"}
      <> enumDirective' 'Delphi Describe {text = "city of oracle"}
      <> enumDirective' 'ARgos Describe {text = "city of argonauts"}
      <> enumDirective' 'Sparta Rename {newName = "sparta"}
      <> enumDirective' 'Delphi Rename {newName = "delphi"}
      <> enumDirective' 'Athens Rename {newName = "_athens"}
      <> enumDirective' 'CORINTH__UGLY_ENUM_NAME Rename {newName = "corinth"}
      <> enumDirective' 'ARgos Rename {newName = "argos"}

data Query (m :: Type -> Type) = Query
  { cities :: [City],
    printCities :: Arg "cities" [City] -> m Text
  }
  deriving (Generic, GQLType)

root :: RootResolver IO () Query Undefined Undefined
root =
  defaultRootResolver
    { queryResolver =
        Query
          { cities =
              [ Athens,
                Sparta,
                CORINTH__UGLY_ENUM_NAME,
                Delphi,
                ARgos
              ],
            printCities = pure . pack . show . argValue
          }
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter root
