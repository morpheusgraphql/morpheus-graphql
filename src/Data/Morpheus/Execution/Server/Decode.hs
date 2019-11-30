{-# LANGUAGE NamedFieldPuns #-}
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
                                                , INPUT
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
  decodeKind _ = withEnum (fmap to . decodeRep . (, initCont) . Enum)

-- INPUT_UNION
instance (Generic a, DecodeRep (Rep a)) => DecodeKind AUTO a where
  decodeKind _ = fmap to . decodeRep . (, initCont)

-- INPUT_OBJECT
instance DecodeObject a => DecodeKind INPUT_OBJECT a where
  decodeKind _ = withObject decodeObject

-- INPUT_UNION
instance (Generic a, DecodeRep (Rep a)) => DecodeKind INPUT_UNION a where
  decodeKind _ = fmap to . decodeRep . (, initCont)

instance (Generic a, DecodeRep (Rep a)) => DecodeKind INPUT a where
  decodeKind _ = fmap to . decodeRep . (, initCont)

-- GENERIC
decodeArguments :: DecodeObject p => Arguments -> Validation p
decodeArguments = decodeObject . fmap toObject
  where toObject (x, y) = (x, argumentValue y)

class DecodeObject a where
  decodeObject :: Object -> Validation a

instance {-# OVERLAPPABLE #-} (Generic a, DecodeRep (Rep a)) => DecodeObject a where
  decodeObject = fmap to . decodeRep . (, initCont) . Object

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


data Tag = D_CONS | D_UNION deriving (Eq ,Ord)

initCont = Cont D_CONS ""

data Cont = Cont {
  contKind:: Tag,
  typeName :: Name
}

data Info = Info {
  kind :: Tag,
  tagName :: [Name]
}

instance Semigroup Info where
  Info D_UNION t1 <> Info _       t2 = Info D_UNION (t1 <> t2)
  Info _       t1 <> Info D_UNION t2 = Info D_UNION (t1 <> t2)
  Info D_CONS  t1 <> Info D_CONS  t2 = Info D_CONS (t1 <> t2)

--
-- GENERICS
--
class DecodeRep f where
  tags :: Proxy f -> Name -> Info
  decodeRep :: (Value,Cont) -> Validation (f a)

instance (Datatype d, DecodeRep f) => DecodeRep (M1 D d f) where
  tags _ = tags (Proxy @f)
  decodeRep (x, y) = M1 <$> decodeRep
    (x, y { typeName = pack $ datatypeName (undefined :: (M1 D d f a)) })

instance (DecodeRep a, DecodeRep b) => DecodeRep (a :+: b) where
  tags _ = tags (Proxy @a) <> tags (Proxy @b)
  decodeRep = __decode
   where

    __decode (Object obj, cont) = withUnion handleUnion obj
     where
      handleUnion name unions object
        | [name] == l1 = L1 <$> decodeRep (Object object, ctx)
        | [name] == r1 = R1 <$> decodeRep (Object object, ctx)
        | otherwise = decideUnion (l1, decodeRep)
                                  (r1, decodeRep)
                                  name
                                  (Object unions, ctx)
      l1  = tagName l1t
      r1  = tagName r1t
      l1t = tags (Proxy @a) (typeName cont)
      r1t = tags (Proxy @b) (typeName cont)
      ctx = cont { contKind = kind (r1t <> r1t) }
    __decode (Enum name, cxt) = decideUnion
      (tagName $ tags (Proxy @a) (typeName cxt), decodeRep)
      (tagName $ tags (Proxy @b) (typeName cxt), decodeRep)
      name
      (Enum name, cxt)
    __decode _ = internalError "lists and scalars are not allowed in Union"

instance (Constructor c, DecodeFields a) => DecodeRep (M1 C c a) where
  decodeRep = fmap M1 . decodeFields
  tags _ baseName = getTag (refType (Proxy @a))
   where
    getTag (Just memberRef)
      | not (conIsRecord unsafeType) && isNamespacedConstraint memberRef = Info
        { kind    = D_UNION
        , tagName = [memberRef]
        }
      | otherwise = Info { kind = D_CONS, tagName = [consName] }
    getTag Nothing = Info { kind = D_CONS, tagName = [consName] }
    --------
    consName = pack $ conName unsafeType
    ----------
    isNamespacedConstraint x = baseName <> x == consName
    --------------------------
    unsafeType :: (M1 C c U1 x)
    unsafeType = undefined

class DecodeFields f where
  refType :: Proxy f -> Maybe Name
  decodeFields :: (Value,Cont) -> Validation (f a)

instance (DecodeFields f, DecodeFields g) => DecodeFields (f :*: g) where
  refType _ = Nothing
  decodeFields gql = (:*:) <$> decodeFields gql <*> decodeFields gql

instance (Selector s, GQLType a, Decode a) => DecodeFields (M1 S s (K1 i a)) where
  refType _ = Just $ __typeName (Proxy @a)
  decodeFields (value, Cont { contKind })
    | contKind == D_UNION = M1 . K1 <$> decode value
    | otherwise           = __decode value
   where
    __decode  = fmap (M1 . K1) . decodeRec
    fieldName = pack $ selName (undefined :: M1 S s f a)
    decodeRec = withObject (decodeFieldWith decode fieldName)

instance DecodeFields U1 where
  refType _ = Nothing
  decodeFields _ = pure U1
