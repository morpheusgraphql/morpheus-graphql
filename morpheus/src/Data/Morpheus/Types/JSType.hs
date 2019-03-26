{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Types.JSType
  ( JSType(..)
  , Scalar(..)
  ) where

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
  deriving (Show, Generic)

--toJSValue :: Scalar -> JSType
--toJSValue (Int x)     = JSInt x
--toJSValue (Float x)   = JSFloat x
--toJSValue (String x)  = JSString x
--toJSValue (Boolean x) = JSBool x
--toScalar :: JSType -> Scalar
--toScalar (JSInt x)    = Int x
--toScalar (JSFloat x)  = Float x
--toScalar (JSString x) = String x
--toScalar (JSBool x)   = Boolean x
instance A.ToJSON Scalar where
  toEncoding (Float x)   = A.toEncoding x
  toEncoding (Int x)     = A.toEncoding x
  toEncoding (Boolean x) = A.toEncoding x
  toEncoding (String x)  = A.toEncoding x

data JSType
  = JSObject [(Text, JSType)]
  | JSList [JSType]
  | JSEnum Text
  | Scalar Scalar
  | JSNull
  deriving (Show, Generic)

instance A.ToJSON JSType where
  toEncoding JSNull = A.toEncoding A.Null
  toEncoding (JSEnum x) = A.toEncoding x
  toEncoding (JSList x) = A.toEncoding x
  toEncoding (Scalar x) = A.toEncoding x
  toEncoding (JSObject x) = A.pairs $ foldl1 (<>) $ map encodeField x
    where
      encodeField (key, value) = replaceType key A..= value

replace :: (a, A.Value) -> (a, JSType)
replace (key, val) = (key, replaceValue val)

decodeScientific :: Scientific -> Scalar
decodeScientific v =
  case floatingOrInteger v of
    Left float -> Float float
    Right int  -> Int int

replaceValue :: A.Value -> JSType
replaceValue (A.Bool v)   = Scalar $ Boolean v
replaceValue (A.Number v) = Scalar $ decodeScientific v
replaceValue (A.String v) = Scalar $ String v
replaceValue (A.Object v) = JSObject $ map replace (M.toList v)
replaceValue (A.Array li) = JSList (map replaceValue (V.toList li))
replaceValue A.Null       = JSNull

instance A.FromJSON JSType where
  parseJSON = pure . replaceValue
