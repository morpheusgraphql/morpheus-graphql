{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Types.Internal.Value
  ( Value(..)
  , ScalarValue(..)
  , replaceValue
  , decodeScientific
  , convertToJSONName
  , convertToHaskellName
  ) where

import qualified Data.Aeson          as A (FromJSON (..), ToJSON (..), Value (..), object, pairs, (.=))
import qualified Data.HashMap.Strict as M (toList)
import           Data.Scientific     (Scientific, floatingOrInteger)
import           Data.Semigroup      ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V (toList)
import           GHC.Generics        (Generic)

isReserved :: Text -> Bool
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
  deriving (Show, Generic)

instance A.ToJSON ScalarValue where
  toEncoding (Float x)   = A.toEncoding x
  toEncoding (Int x)     = A.toEncoding x
  toEncoding (Boolean x) = A.toEncoding x
  toEncoding (String x)  = A.toEncoding x

data Value
  = Object [(Text, Value)]
  | List [Value]
  | Enum Text
  | Scalar ScalarValue
  | Null
  deriving (Show, Generic)

instance A.ToJSON Value where
  toEncoding Null = A.toEncoding A.Null
  toEncoding (Enum x) = A.toEncoding x
  toEncoding (List x) = A.toEncoding x
  toEncoding (Scalar x) = A.toEncoding x
  toEncoding (Object []) = A.toEncoding $ A.object []
  toEncoding (Object x) = A.pairs $ foldl1 (<>) $ map encodeField x
    where
      encodeField (key, value) = convertToJSONName key A..= value

replace :: (a, A.Value) -> (a, Value)
replace (key, val) = (key, replaceValue val)

decodeScientific :: Scientific -> ScalarValue
decodeScientific v =
  case floatingOrInteger v of
    Left float -> Float float
    Right int  -> Int int

replaceValue :: A.Value -> Value
replaceValue (A.Bool v)   = Scalar $ Boolean v
replaceValue (A.Number v) = Scalar $ decodeScientific v
replaceValue (A.String v) = Scalar $ String v
replaceValue (A.Object v) = Object $ map replace (M.toList v)
replaceValue (A.Array li) = List (map replaceValue (V.toList li))
replaceValue A.Null       = Null

instance A.FromJSON Value where
  parseJSON = pure . replaceValue
