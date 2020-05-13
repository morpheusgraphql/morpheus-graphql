{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Types.Internal.AST.Value
  ( Value (..),
    ScalarValue (..),
    Object,
    GQLValue (..),
    replaceValue,
    decodeScientific,
    convertToJSONName,
    convertToHaskellName,
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

import qualified Data.Aeson as A
  ( (.=),
    FromJSON (..),
    ToJSON (..),
    Value (..),
    encode,
    object,
    pairs,
  )
import qualified Data.HashMap.Strict as M
  ( toList,
  )
-- MORPHEUS
import Data.Morpheus.Error.NameCollision
  ( NameCollision (..),
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    GQLError (..),
    Msg (..),
    Name (..),
    Position,
    RAW,
    RESOLVED,
    Ref (..),
    Stage,
    TypeRef,
    TypeRef (..),
    VALID,
  )
import Data.Morpheus.Types.Internal.AST.OrderedMap
  ( OrderedMap,
    foldWithKey,
    unsafeFromValues,
  )
import Data.Morpheus.Types.Internal.Operation
  ( KeyOf (..),
    Listable (..),
  )
import Data.Scientific
  ( Scientific,
    floatingOrInteger,
  )
import Data.Semigroup ((<>))
import Data.Text
  ( Text,
    unpack,
  )
import qualified Data.Text as T
import qualified Data.Vector as V
  ( toList,
  )
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift (..))

isReserved :: Name -> Bool
isReserved "case" = True
isReserved "class" = True
isReserved "data" = True
isReserved "default" = True
isReserved "deriving" = True
isReserved "do" = True
isReserved "else" = True
isReserved "foreign" = True
isReserved "if" = True
isReserved "import" = True
isReserved "in" = True
isReserved "infix" = True
isReserved "infixl" = True
isReserved "infixr" = True
isReserved "instance" = True
isReserved "let" = True
isReserved "module" = True
isReserved "newtype" = True
isReserved "of" = True
isReserved "then" = True
isReserved "type" = True
isReserved "where" = True
isReserved "_" = True
isReserved _ = False
{-# INLINE isReserved #-}

convertToJSONName :: Name -> Text
convertToJSONName (Name hsName)
  | not (T.null hsName) && isReserved (Name name) && (T.last hsName == '\'') = name
  | otherwise = hsName
  where
    name = T.init hsName

convertToHaskellName :: Name -> Name
convertToHaskellName name
  | isReserved name = name <> "'"
  | otherwise = name

-- | Primitive Values for GQLScalar: 'Int', 'Float', 'String', 'Boolean'.
-- for performance reason type 'Text' represents GraphQl 'String' value
data ScalarValue
  = Int Int
  | Float Float
  | String Text
  | Boolean Bool
  deriving (Show, Eq, Generic, Lift)

instance A.ToJSON ScalarValue where
  toJSON (Float x) = A.toJSON x
  toJSON (Int x) = A.toJSON x
  toJSON (Boolean x) = A.toJSON x
  toJSON (String x) = A.toJSON x

instance A.FromJSON ScalarValue where
  parseJSON (A.Bool v) = pure $ Boolean v
  parseJSON (A.Number v) = pure $ decodeScientific v
  parseJSON (A.String v) = pure $ String v
  parseJSON notScalar = fail $ "Expected Scalar got :" <> show notScalar

type family VAR (a :: Stage) :: Stage

type instance VAR RAW = RESOLVED

type instance VAR RESOLVED = RESOLVED

type instance VAR VALID = VALID

data VariableContent (stage :: Stage) where
  DefaultValue :: Maybe ResolvedValue -> VariableContent RESOLVED
  ValidVariableValue :: {validVarContent :: ValidValue} -> VariableContent VALID

instance Lift (VariableContent a) where
  lift (DefaultValue x) = [|DefaultValue x|]
  lift (ValidVariableValue x) = [|ValidVariableValue x|]

deriving instance Show (VariableContent a)

deriving instance Eq (VariableContent a)

data Variable (stage :: Stage) = Variable
  { variableName :: Name,
    variableType :: TypeRef,
    variablePosition :: Position,
    variableValue :: VariableContent (VAR stage)
  }
  deriving (Show, Eq, Lift)

instance KeyOf (Variable s) where
  keyOf = variableName

instance NameCollision (Variable s) where
  nameCollision _ Variable {variableName, variablePosition} =
    GQLError
      { message = "There can Be only One Variable Named " <> msg variableName,
        locations = [variablePosition]
      }

type VariableDefinitions s = OrderedMap (Variable s)

data Value (stage :: Stage) where
  ResolvedVariable :: Ref -> Variable VALID -> Value RESOLVED
  VariableValue :: Ref -> Value RAW
  Object :: Object stage -> Value stage
  List :: [Value stage] -> Value stage
  Enum :: Name -> Value stage
  Scalar :: ScalarValue -> Value stage
  Null :: Value stage

deriving instance Eq (Value s)

data ObjectEntry (s :: Stage) = ObjectEntry
  { entryName :: Name,
    entryValue :: Value s
  }
  deriving (Eq)

instance Show (ObjectEntry s) where
  show (ObjectEntry (Name name) value) = unpack name <> ":" <> show value

instance NameCollision (ObjectEntry s) where
  nameCollision _ ObjectEntry {entryName} =
    GQLError
      { message = "There can Be only One field Named " <> msg entryName,
        locations = []
      }

instance KeyOf (ObjectEntry s) where
  keyOf = entryName

type Object a = OrderedMap FieldName (ObjectEntry a)

type ValidObject = Object VALID

type RawObject = Object RAW

type ResolvedObject = Object RESOLVED

type RawValue = Value RAW

type ValidValue = Value VALID

type ResolvedValue = Value RESOLVED

deriving instance Lift (Value a)

deriving instance Lift (ObjectEntry a)

instance Show (Value a) where
  show Null = "null"
  show (Enum x) = "" <> unpack (readName x)
  show (Scalar x) = show x
  show (ResolvedVariable Ref {refName} Variable {variableValue}) =
    "($" <> unpack (readName refName) <> ": " <> show variableValue <> ") "
  show (VariableValue Ref {refName}) = "$" <> unpack (readName refName) <> " "
  show (Object keys) = "{" <> foldWithKey toEntry "" keys <> "}"
    where
      toEntry :: Name -> ObjectEntry a -> String -> String
      toEntry _ value "" = show value
      toEntry _ value txt = txt <> ", " <> show value
  show (List list) = "[" <> foldl toEntry "" list <> "]"
    where
      toEntry :: String -> Value a -> String
      toEntry "" value = show value
      toEntry txt value = txt <> ", " <> show value

instance Msg (Value a) where
  msg = msg . A.encode

instance A.ToJSON (Value a) where
  toJSON (ResolvedVariable _ Variable {variableValue = ValidVariableValue x}) =
    A.toJSON x
  toJSON (VariableValue Ref {refName}) =
    A.String $ "($ref:" <> readName refName <> ")"
  toJSON Null = A.Null
  toJSON (Enum (Name x)) = A.String x
  toJSON (Scalar x) = A.toJSON x
  toJSON (List x) = A.toJSON x
  toJSON (Object fields) = A.object $ map toEntry (toList fields)
    where
      toEntry (ObjectEntry (Name name) value) = name A..= A.toJSON value

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
    | otherwise = A.pairs $ foldl1 (<>) $ map encodeField (toList ordmap)
    where
      encodeField (ObjectEntry key value) = convertToJSONName key A..= value

decodeScientific :: Scientific -> ScalarValue
decodeScientific v = case floatingOrInteger v of
  Left float -> Float float
  Right int -> Int int

replaceValue :: A.Value -> Value a
replaceValue (A.Bool v) = gqlBoolean v
replaceValue (A.Number v) = Scalar $ decodeScientific v
replaceValue (A.String v) = gqlString v
replaceValue (A.Object v) = gqlObject $ map replace (M.toList v)
  where
    --replace :: (a, A.Value) -> (a, Value a)
    replace (key, val) = (Name key, replaceValue val)
replaceValue (A.Array li) = gqlList (map replaceValue (V.toList li))
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
  gqlObject :: [(Name, a)] -> a

-- build GQL Values for Subscription Resolver
instance GQLValue (Value a) where
  gqlNull = Null
  gqlScalar = Scalar
  gqlBoolean = Scalar . Boolean
  gqlString = Scalar . String
  gqlList = List
  gqlObject = Object . unsafeFromValues . map toEntry
    where
      toEntry (key, value) = ObjectEntry key value
