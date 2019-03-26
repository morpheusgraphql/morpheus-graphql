{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Types.JSType where

import qualified Data.Aeson          as A (FromJSON (..), ToJSON (..), Value (..), pairs, (.=))
import qualified Data.HashMap.Strict as M (toList)
import           Data.Scientific     (Scientific, floatingOrInteger)
import           Data.Text           (Text)
import qualified Data.Vector         as V (toList)
import           GHC.Generics        (Generic)

replaceType :: Text -> Text
replaceType "_type" = "type"
replaceType x       = x

data Scalar
  = Int Int
  | Float Float
  | String Text
  | Boolean Bool

data JSType
  = JSObject [(Text, JSType)]
  | JSList [JSType]
  | JSEnum Text
  | JSInt Int
  | JSFloat Float
  | JSBool Bool
  | JSString Text
  | JSNull
  deriving (Show, Generic)

instance A.ToJSON JSType where
  toEncoding JSNull = toEncoding A.Null
  toEncoding (JSEnum x) = toEncoding x
  toEncoding (JSFloat x) = toEncoding x
  toEncoding (JSInt x) = toEncoding x
  toEncoding (JSBool x) = toEncoding x
  toEncoding (JSString x) = toEncoding x
  toEncoding (JSList x) = toEncoding x
  toEncoding (JSObject x) = pairs $ foldl1 (<>) $ map encodeField x
    where
      encodeField (key, value) = replaceType key .= value

replace :: (a, Value) -> (a, JSType)
replace (key, val) = (key, replaceValue val)

decodeScientific :: Scientific -> JSType
decodeScientific v =
  case floatingOrInteger v of
    Left float -> JSFloat float
    Right int  -> JSInt int

replaceValue :: A.Value -> JSType
replaceValue (A.Bool v)   = JSBool v
replaceValue (A.Number v) = decodeScientific v
replaceValue (A.String v) = JSString v
replaceValue (A.Object v) = JSObject $ map replace (M.toList v)
replaceValue (A.Array li) = JSList (map replaceValue (V.toList li))
replaceValue A.Null       = JSNull

instance A.FromJSON JSType where
  parseJSON = pure . replaceValue
