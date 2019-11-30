{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TupleSections         #-}

module Data.Morpheus.Execution.Server.Decode
  ( decodeArguments
  , Decode(..)
  , DecodeObject(..)
  )
where

import           Data.Proxy                     ( Proxy(..) )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( pack )
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Error.Internal   ( internalTypeMismatch
                                                , internalError
                                                )
import           Data.Morpheus.Execution.Internal.Decode
                                                ( decodeFieldWith
                                                , withEnum
                                                , withList
                                                , withMaybe
                                                , withObject
                                                , withUnion
                                                )
import           Data.Morpheus.Kind             ( ENUM
                                                , GQL_KIND
                                                , INPUT_OBJECT
                                                , INPUT_UNION
                                                , SCALAR
                                                , AUTO
                                                )
import           Data.Morpheus.Types.GQLScalar  ( GQLScalar(..)
                                                , toScalar
                                                )
import           Data.Morpheus.Types.GQLType    ( GQLType(KIND, __typeName) )
import           Data.Morpheus.Types.Internal.AST
                                                ( Name
                                                , Argument(..)
                                                , Arguments
                                                , Object
                                                , Value(..)
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation
                                                , Failure(..)
                                                )


-- | Decode GraphQL query arguments and input values
class Decode a where
  decode :: Value -> Validation a

instance {-# OVERLAPPABLE #-} DecodeKind (KIND a) a => Decode a where
  decode = decodeKind (Proxy @(KIND a))

instance Decode a => Decode (Maybe a) where
  decode = withMaybe decode

instance Decode a => Decode [a] where
  decode = withList decode

-- | Decode GraphQL type with Specific Kind
class DecodeKind (kind :: GQL_KIND) a where
  decodeKind :: Proxy kind -> Value -> Validation a

-- SCALAR
instance (GQLScalar a) => DecodeKind SCALAR a where
  decodeKind _ value = case toScalar value >>= parseValue of
    Right scalar       -> return scalar
    Left  errorMessage -> internalTypeMismatch errorMessage value

-- ENUM
instance (Generic a, DecodeRep (Rep a)) => DecodeKind ENUM a where
  decodeKind _ = withEnum (fmap to . decodeRep . (, False) . Enum)

-- INPUT_UNION
instance (Generic a, DecodeRep (Rep a)) => DecodeKind AUTO a where
  decodeKind _ = fmap to . decodeRep . (, False)

-- INPUT_OBJECT
instance DecodeObject a => DecodeKind INPUT_OBJECT a where
  decodeKind _ = withObject decodeObject

-- INPUT_UNION
instance (Generic a, DecodeRep (Rep a)) => DecodeKind INPUT_UNION a where
  decodeKind _ = fmap to . decodeRep . (, False)

-- GENERIC
decodeArguments :: DecodeObject p => Arguments -> Validation p
decodeArguments = decodeObject . fmap toObject
  where toObject (x, y) = (x, argumentValue y)

class DecodeObject a where
  decodeObject :: Object -> Validation a

instance {-# OVERLAPPABLE #-} (Generic a, DecodeRep (Rep a)) => DecodeObject a where
  decodeObject = fmap to . decodeRep . (, False) . Object

-- data Inpuz  =
--    InputHuman Human  -- direct link: { __typename: Human, field:"" }
--   
--   | InputRecord { name :: Text, age :: Int } -- { __typename: InputRecord, name:"" , age:1 }
--    
--   | IndexedType Int Text  -- { __typename: InputRecord, _0:2 , _1:""  }
--   
--   | Zeus                 -- "Zeus"
--   | Cronus 
--     deriving (Generic, GQLType)

-- object and union: { __typename: name , field: some field }   
-- | EnumValue | 

decideUnion
  :: ([Name], value -> Validation (f1 a))
  -> ([Name], value -> Validation (f2 a))
  -> Name
  -> value
  -> Validation ((:+:) f1 f2 a)
decideUnion (left, f1) (right, f2) name value
  | name `elem` left
  = L1 <$> f1 value
  | name `elem` right
  = R1 <$> f2 value
  | otherwise
  = failure $ "Constructor \"" <> name <> "\" could not find in Union"

--
-- GENERICS
--
class DecodeRep f where
  enums :: Proxy f -> [Name]
  tags :: Proxy f -> [Name]
  decodeRep :: (Value,Bool) -> Validation (f a)

instance (Datatype d, DecodeRep f) => DecodeRep (M1 D d f) where
  enums _ = enums (Proxy @f)
  tags _ = tags (Proxy @f)
  decodeRep = fmap M1 . decodeRep

instance (DecodeRep a, DecodeRep b) => DecodeRep (a :+: b) where
  enums _ = enums (Proxy @a) <> enums (Proxy @b)
  tags _ = tags (Proxy @a) <> tags (Proxy @b)
  decodeRep = __decode
   where
    __decode (Object obj, _) = withUnion handleUnion obj
     where
      handleUnion name unions object
        | [name] == l1 = L1 <$> decodeRep (Object object, True)
        | [name] == r1 = R1 <$> decodeRep (Object object, True)
        | otherwise = decideUnion (l1, decodeRep)
                                  (r1, decodeRep)
                                  name
                                  (Object unions, True)
      l1 = tags $ Proxy @a
      r1 = tags $ Proxy @b
    __decode (Enum name, cxt) = decideUnion (enums $ Proxy @a, decodeRep)
                                            (enums $ Proxy @b, decodeRep)
                                            name
                                            (Enum name, cxt)
    __decode _ = internalError "lists and scalars are not allowed in Union"

instance (Constructor c, DecodeFields f) => DecodeRep (M1 C c f) where
  enums _ = [pack $ conName (undefined :: (M1 C c U1 x))]
  tags _ = fieldTags (Proxy @f)
  decodeRep = fmap M1 . decodeFields

class DecodeFields f where
  fieldTags :: Proxy f -> [Name]
  decodeFields :: (Value,Bool) -> Validation (f a)

instance (DecodeFields f, DecodeFields g) => DecodeFields (f :*: g) where
  fieldTags _ = []
  decodeFields gql = (:*:) <$> decodeFields gql <*> decodeFields gql

instance (Selector s, GQLType a, Decode a) => DecodeFields (M1 S s (K1 i a)) where
  fieldTags _ = [__typeName (Proxy @a)]
  decodeFields (value, isUnion) | isUnion   = M1 . K1 <$> decode value
                                | otherwise = __decode value
   where
    __decode  = fmap (M1 . K1) . decodeRec
    fieldName = pack $ selName (undefined :: M1 S s f a)
    decodeRec = withObject (decodeFieldWith decode fieldName)

instance DecodeFields U1 where
  fieldTags _ = []
  decodeFields _ = pure U1
