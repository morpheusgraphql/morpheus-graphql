{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Value
  ( Value (..),
    ScalarValue (..),
    Object,
    GQLValue (..),
    replaceValue,
    decodeScientific,
    RawValue,
    ValidValue,
    RawObject,
    ValidObject,
    Variable (..),
    ResolvedValue,
    ResolvedObject,
    VariableContent (..),
    ObjectEntry (..),
    VariableDefinitions,
  )
where

-- MORPHEUS

import Control.Applicative (pure)
import Control.Monad.Fail (MonadFail (fail))
import qualified Data.Aeson as A
  ( (.=),
    FromJSON (..),
    ToJSON (..),
    Value (..),
    object,
    pairs,
  )
import Data.Either (Either (..))
import Data.Foldable (foldl, foldl1, null)
import Data.Functor (fmap)
import qualified Data.HashMap.Lazy as M
  ( toList,
  )
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Morpheus.Error.NameCollision
  ( NameCollision (..),
  )
import Data.Morpheus.Internal.Utils
  ( KeyOf (..),
    elems,
    mapTuple,
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    Rendering,
    fromText,
    renderGQL,
    space,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    FieldName (..),
    Msg (..),
    Position,
    Ref (..),
    TypeName (..),
    TypeRef,
    ValidationError (..),
    msgValidation,
  )
import Data.Morpheus.Types.Internal.AST.OrdMap
  ( OrdMap,
    unsafeFromValues,
  )
import Data.Morpheus.Types.Internal.AST.Stage
  ( CONST,
    CONST_OR_VALID,
    RAW,
    Stage,
    VALID,
  )
import Data.Scientific
  ( Scientific,
    floatingOrInteger,
  )
import Data.Semigroup ((<>))
import Data.Text
  ( Text,
  )
import qualified Data.Vector as V
  ( toList,
  )
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift (..))
import Prelude
  ( ($),
    (.),
    Bool,
    Eq,
    Float,
    Int,
    Show (..),
    otherwise,
  )

-- | Primitive Values for GQLScalar: 'Int', 'Float', 'String', 'Boolean'.
-- for performance reason type 'Text' represents GraphQl 'String' value
data ScalarValue
  = Int Int
  | Float Float
  | String Text
  | Boolean Bool
  | Value A.Value
  deriving (Show, Eq, Generic, Lift)

instance RenderGQL ScalarValue where
  render (Int x) = render x
  render (Float x) = render x
  render (String x) = render x
  render (Boolean x) = render x
  render (Value x) = render x

instance A.ToJSON ScalarValue where
  toJSON (Float x) = A.toJSON x
  toJSON (Int x) = A.toJSON x
  toJSON (Boolean x) = A.toJSON x
  toJSON (String x) = A.toJSON x
  toJSON (Value x) = A.toJSON x

instance A.FromJSON ScalarValue where
  parseJSON (A.Bool v) = pure $ Boolean v
  parseJSON (A.Number v) = pure $ decodeScientific v
  parseJSON (A.String v) = pure $ String v
  parseJSON notScalar = fail $ "Expected Scalar got :" <> show notScalar

data VariableContent (stage :: Stage) where
  DefaultValue :: Maybe ResolvedValue -> VariableContent CONST
  ValidVariableValue :: {validVarContent :: ValidValue} -> VariableContent VALID

instance Lift (VariableContent a) where
  lift (DefaultValue x) = [|DefaultValue x|]
  lift (ValidVariableValue x) = [|ValidVariableValue x|]

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (DefaultValue x) = [||DefaultValue x||]
  liftTyped (ValidVariableValue x) = [||ValidVariableValue x||]
#endif
deriving instance Show (VariableContent a)

deriving instance Eq (VariableContent a)

data Variable (stage :: Stage) = Variable
  { variableName :: FieldName,
    variableType :: TypeRef,
    variablePosition :: Position,
    variableValue :: VariableContent (CONST_OR_VALID stage)
  }
  deriving (Show, Eq, Lift)

instance KeyOf FieldName (Variable s) where
  keyOf = variableName

instance NameCollision (Variable s) where
  nameCollision Variable {variableName, variablePosition} =
    ValidationError
      { validationMessage = "There can Be only One Variable Named " <> msg variableName,
        validationLocations = [variablePosition]
      }

type VariableDefinitions s = OrdMap FieldName (Variable s)

data Value (stage :: Stage) where
  ResolvedVariable :: Ref -> Variable VALID -> Value CONST
  VariableValue :: Ref -> Value RAW
  Object :: Object stage -> Value stage
  List :: [Value stage] -> Value stage
  Enum :: TypeName -> Value stage
  Scalar :: ScalarValue -> Value stage
  Null :: Value stage

deriving instance Show (Value a)

deriving instance Eq (Value s)

data ObjectEntry (s :: Stage) = ObjectEntry
  { entryName :: FieldName,
    entryValue :: Value s
  }
  deriving (Eq, Show)

instance RenderGQL (ObjectEntry a) where
  render (ObjectEntry (FieldName name) value) = fromText name <> ": " <> render value

instance NameCollision (ObjectEntry s) where
  nameCollision ObjectEntry {entryName} =
    "There can Be only One field Named " <> msgValidation entryName :: ValidationError

instance KeyOf FieldName (ObjectEntry s) where
  keyOf = entryName

type Object a = OrdMap FieldName (ObjectEntry a)

type ValidObject = Object VALID

type RawObject = Object RAW

type ResolvedObject = Object CONST

type RawValue = Value RAW

type ValidValue = Value VALID

type ResolvedValue = Value CONST

deriving instance Lift (Value a)

deriving instance Lift (ObjectEntry a)

instance RenderGQL (Value a) where
  render (ResolvedVariable Ref {refName} _) = "$" <> render refName
  render (VariableValue Ref {refName}) = "$" <> render refName <> " "
  render Null = "null"
  render (Enum x) = render x
  render (Scalar x) = render x
  render (Object keys) = "{" <> space <> (fromMaybe "" renderX) <> space <> "}"
    where
      renderX = foldl toEntry Nothing (elems keys)
      toEntry :: Maybe Rendering -> ObjectEntry a -> Maybe Rendering
      toEntry Nothing value = Just (render value)
      toEntry (Just txt) value = Just (txt <> ", " <> render value)
  render (List list) = "[" <> (fromMaybe "" renderX) <> "]"
    where
      renderX = foldl toEntry Nothing list
      toEntry :: Maybe Rendering -> Value a -> Maybe Rendering
      toEntry Nothing value = Just (render value)
      toEntry (Just txt) value = Just (txt <> ", " <> render value)

-- render = pack . BS.unpack . A.encode

instance Msg (Value a) where
  msg = msg . renderGQL

instance A.ToJSON (Value a) where
  toJSON (ResolvedVariable _ Variable {variableValue = ValidVariableValue x}) =
    A.toJSON x
  toJSON (VariableValue Ref {refName}) =
    A.String $ "($ref:" <> readName refName <> ")"
  toJSON Null = A.Null
  toJSON (Enum (TypeName x)) = A.String x
  toJSON (Scalar x) = A.toJSON x
  toJSON (List x) = A.toJSON x
  toJSON (Object fields) = A.object $ fmap toEntry (elems fields)
    where
      toEntry (ObjectEntry (FieldName name) value) = name A..= A.toJSON value

  -------------------------------------------
  toEncoding (ResolvedVariable _ Variable {variableValue = ValidVariableValue x}) =
    A.toEncoding x
  toEncoding (VariableValue Ref {refName}) =
    A.toEncoding $ "($ref:" <> refName <> ")"
  toEncoding Null = A.toEncoding A.Null
  toEncoding (Enum x) = A.toEncoding x
  toEncoding (Scalar x) = A.toEncoding x
  toEncoding (List x) = A.toEncoding x
  toEncoding (Object ordmap)
    | null ordmap = A.toEncoding $ A.object []
    | otherwise = A.pairs $ foldl1 (<>) $ fmap encodeField (elems ordmap)
    where
      encodeField (ObjectEntry (FieldName key) value) = key A..= value

decodeScientific :: Scientific -> ScalarValue
decodeScientific v = case floatingOrInteger v of
  Left float -> Float float
  Right int -> Int int

replaceValue :: A.Value -> Value a
replaceValue (A.Bool v) = gqlBoolean v
replaceValue (A.Number v) = Scalar $ decodeScientific v
replaceValue (A.String v) = gqlString v
replaceValue (A.Object v) =
  gqlObject $
    fmap
      (mapTuple FieldName replaceValue)
      (M.toList v)
replaceValue (A.Array li) = gqlList (fmap replaceValue (V.toList li))
replaceValue A.Null = gqlNull

instance A.FromJSON (Value a) where
  parseJSON = pure . replaceValue

-- DEFAULT VALUES
class GQLValue a where
  gqlNull :: a
  gqlScalar :: ScalarValue -> a
  gqlBoolean :: Bool -> a
  gqlString :: Text -> a
  gqlList :: [a] -> a
  gqlObject :: [(FieldName, a)] -> a

-- build GQL Values for Subscription Resolver
instance GQLValue (Value a) where
  gqlNull = Null
  gqlScalar = Scalar
  gqlBoolean = Scalar . Boolean
  gqlString = Scalar . String
  gqlList = List
  gqlObject = Object . unsafeFromValues . fmap toEntry
    where
      toEntry (key, value) = ObjectEntry key value
