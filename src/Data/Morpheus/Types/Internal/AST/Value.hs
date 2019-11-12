{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Data.Morpheus.Types.Internal.AST.Value
  ( Value(..)
  , ScalarValue(..)
  , Object
  , GQLValue(..)
  , replaceValue
  , decodeScientific
  , convertToJSONName
  , convertToHaskellName
  ) where

import qualified Data.Aeson                      as A (FromJSON (..), ToJSON (..), Value (..), object, pairs, (.=))
import           Data.Function                   ((&))
import qualified Data.HashMap.Strict             as M (toList)
import           Data.Scientific                 (Scientific, floatingOrInteger)
import           Data.Semigroup                  ((<>))
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Vector                     as V (toList)
import           GHC.Generics                    (Generic)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

-- MORPHEUS
import           Data.Morpheus.Types.Internal.TH (apply, liftText, liftTextMap)

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
  where
    name = T.init hsName

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

instance Lift ScalarValue where
  lift (String n)  = apply 'String [liftText n]
  lift (Int n)     = apply 'Int [lift n]
  lift (Float n)   = apply 'Float [lift n]
  lift (Boolean n) = apply 'Boolean [lift n]

instance A.ToJSON ScalarValue where
  toJSON (Float x)   = A.toJSON x
  toJSON (Int x)     = A.toJSON x
  toJSON (Boolean x) = A.toJSON x
  toJSON (String x)  = A.toJSON x

instance A.FromJSON ScalarValue where
  parseJSON (A.Bool v)   = pure $ Boolean v
  parseJSON (A.Number v) = pure $ decodeScientific v
  parseJSON (A.String v) = pure $ String v
  parseJSON notScalar    = fail $ "Expected Scalar got :" <> show notScalar

instance Lift Value where
  lift (Object ls) = apply 'Object [liftTextMap ls]
  lift (List n)    = apply 'List [lift n]
  lift (Enum n)    = apply 'Enum [liftText n]
  lift (Scalar n)  = apply 'Scalar [lift n]
  lift Null        = varE 'Null

type Object = [(Text, Value)]

data Value
  = Object Object
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

decodeScientific :: Scientific -> ScalarValue
decodeScientific v =
  case floatingOrInteger v of
    Left float -> Float float
    Right int  -> Int int

replaceValue :: A.Value -> Value
replaceValue (A.Bool v) = gqlBoolean v
replaceValue (A.Number v) = Scalar $ decodeScientific v
replaceValue (A.String v) = gqlString v
replaceValue (A.Object v) = gqlObject $ map replace (M.toList v)
  where
    replace :: (a, A.Value) -> (a, Value)
    replace (key, val) = (key, replaceValue val)
replaceValue (A.Array li) = gqlList (map replaceValue (V.toList li))
replaceValue A.Null = gqlNull

instance A.FromJSON Value where
  parseJSON = pure . replaceValue

-- DEFAULT VALUES
class GQLValue a where
  gqlNull :: a
  gqlScalar :: ScalarValue -> a
  gqlBoolean :: Bool -> a
  gqlString :: Text -> a
  gqlList :: [a] -> a
  gqlObject :: [(Text, a)] -> a

-- build GQL Values for Subscription Resolver
instance GQLValue Value where
  gqlNull = Null
  gqlScalar = Scalar
  gqlBoolean = Scalar . Boolean
  gqlString = Scalar . String
  gqlList = List
  gqlObject = Object

instance Monad m => GQLValue (m Value) where
  gqlNull = pure gqlNull
  gqlScalar = pure . gqlScalar
  gqlBoolean = pure . gqlBoolean
  gqlString = pure . gqlString
  -----------------------------------------
  -- listValue :: [m Value] -> m Value
  gqlList = fmap gqlList . sequence
  -----------------------------------------
  -- objectValue :: [(Text, m Value )] -> m Value
  gqlObject = fmap gqlObject . traverse keyVal
    where
      keyVal :: Monad m => (Text, m Value) -> m (Text, Value)
      keyVal (key, valFunc) = (key, ) <$> valFunc

-- build GQL Values for Subscription Resolver
instance Monad m => GQLValue (args -> m Value) where
  gqlNull = const gqlNull
  gqlScalar = const . gqlScalar
  gqlBoolean = pure . gqlBoolean
  gqlString = const . gqlString
  ----------------------------------------
   -- listValue :: [args -> m Value] -> ( args -> m Value )
  gqlList res args = gqlList <$> traverse (args &) res
  ----------------------------------------
  -- objectValue :: [(Text, args -> m Value )] -> ( args -> m Value )
  gqlObject res args = gqlObject <$> traverse keyVal res
    where
      keyVal :: Monad m => (Text, args -> m Value) -> m (Text, Value)
      keyVal (key, valFunc) = (key, ) <$> valFunc args
