{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TemplateHaskell     #-}

module Data.Morpheus.Types.Internal.AST.Value
  ( Value(..)
  , VariableValue(..)
  , ScalarValue(..)
  , Object
  , GQLValue(..)
  , replaceValue
  , decodeScientific
  , convertToJSONName
  , convertToHaskellName
  , RawValue
  , ValidValue
  )
where

import qualified Data.Aeson                    as A
                                                ( FromJSON(..)
                                                , ToJSON(..)
                                                , Value(..)
                                                , object
                                                , pairs
                                                , (.=)
                                                )
import qualified Data.HashMap.Strict           as M
                                                ( toList )
import           Data.Scientific                ( Scientific
                                                , floatingOrInteger
                                                )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
                                                ( toList )
import           GHC.Generics                   ( Generic )
import           Instances.TH.Lift              ( )
import           Language.Haskell.TH.Syntax     ( Lift(..) )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Collection
                                                , Ref(..)
                                                , Name
                                                , RAW
                                                , VALID
                                                )


isReserved :: Name -> Bool
isReserved "case"     = True
isReserved "class"    = True
isReserved "data"     = True
isReserved "default"  = True
isReserved "deriving" = True
isReserved "do"       = True
isReserved "else"     = True
isReserved "foreign"  = True
isReserved "if"       = True
isReserved "import"   = True
isReserved "in"       = True
isReserved "infix"    = True
isReserved "infixl"   = True
isReserved "infixr"   = True
isReserved "instance" = True
isReserved "let"      = True
isReserved "module"   = True
isReserved "newtype"  = True
isReserved "of"       = True
isReserved "then"     = True
isReserved "type"     = True
isReserved "where"    = True
isReserved "_"        = True
isReserved _          = False

{-# INLINE isReserved #-}
convertToJSONName :: Text -> Text
convertToJSONName hsName
  | not (T.null hsName) && isReserved name && (T.last hsName == '\'') = name
  | otherwise = hsName
  where name = T.init hsName

convertToHaskellName :: Text -> Text
convertToHaskellName name | isReserved name = name <> "'"
                          | otherwise       = name

-- | Primitive Values for GQLScalar: 'Int', 'Float', 'String', 'Boolean'.
-- for performance reason type 'Text' represents GraphQl 'String' value
data ScalarValue
  = Int Int
  | Float Float
  | String Text
  | Boolean Bool
  deriving (Show, Generic,Lift)


instance A.ToJSON ScalarValue where
  toJSON (Float   x) = A.toJSON x
  toJSON (Int     x) = A.toJSON x
  toJSON (Boolean x) = A.toJSON x
  toJSON (String  x) = A.toJSON x

instance A.FromJSON ScalarValue where
  parseJSON (A.Bool   v) = pure $ Boolean v
  parseJSON (A.Number v) = pure $ decodeScientific v
  parseJSON (A.String v) = pure $ String v
  parseJSON notScalar    = fail $ "Expected Scalar got :" <> show notScalar


type Object = Collection Value


data Value
  = Object Object
  | List [Value]
  | Enum Text
  | Scalar ScalarValue
  | Null
  deriving (Show, Generic,Lift)

instance A.ToJSON Value where
  toEncoding Null        = A.toEncoding A.Null
  toEncoding (Enum   x ) = A.toEncoding x
  toEncoding (List   x ) = A.toEncoding x
  toEncoding (Scalar x ) = A.toEncoding x
  toEncoding (Object []) = A.toEncoding $ A.object []
  toEncoding (Object x ) = A.pairs $ foldl1 (<>) $ map encodeField x
    where encodeField (key, value) = convertToJSONName key A..= value

decodeScientific :: Scientific -> ScalarValue
decodeScientific v = case floatingOrInteger v of
  Left  float -> Float float
  Right int   -> Int int

replaceValue :: A.Value -> Value
replaceValue (A.Bool   v) = gqlBoolean v
replaceValue (A.Number v) = Scalar $ decodeScientific v
replaceValue (A.String v) = gqlString v
replaceValue (A.Object v) = gqlObject $ map replace (M.toList v)
 where
  replace :: (a, A.Value) -> (a, Value)
  replace (key, val) = (key, replaceValue val)
replaceValue (A.Array li) = gqlList (map replaceValue (V.toList li))
replaceValue A.Null       = gqlNull

instance A.FromJSON Value where
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
instance GQLValue Value where
  gqlNull    = Null
  gqlScalar  = Scalar
  gqlBoolean = Scalar . Boolean
  gqlString  = Scalar . String
  gqlList    = List
  gqlObject  = Object



data VariableValue (valid :: Bool) where
    VariableObject ::VariableObject -> VariableValue RAW
    VariableList ::[VariableValue 'False] -> VariableValue RAW
    VariableValue ::Ref -> VariableValue RAW
    ConstantValue ::{ constantValue :: Value } -> VariableValue valid

type VariableObject = Collection RawValue
type RawValue = VariableValue RAW
type ValidValue = VariableValue VALID

instance Show (VariableValue a) where

instance Lift (VariableValue a) where
  lift (VariableObject x) = [| VariableObject x |]
  lift (VariableList   x) = [| VariableList x |]
  lift (VariableValue  x) = [| VariableValue x |]
  lift (ConstantValue  x) = [| ConstantValue x |]
