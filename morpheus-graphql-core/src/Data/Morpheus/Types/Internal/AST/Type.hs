{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Type
  ( DataTypeWrapper (..),
    TypeRef (..),
    TypeWrapper (..),
    Nullable (..),
    Strictness (..),
    TypeKind (..),
    isStronger,
    mkTypeRef,
    toGQLWrapper,
    toTypeRef,
    mkBaseType,
    mkMaybeType,
  )
where

import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    render,
    renderGQL,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName (..),
    Msg (..),
    OperationType,
    TypeName (..),
  )
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
import Language.Haskell.TH.Syntax (Lift (..))
import Relude hiding
  ( ByteString,
    decodeUtf8,
    intercalate,
  )

-- Kind
-----------------------------------------------------------------------------------
data TypeKind
  = KindScalar
  | KindObject (Maybe OperationType)
  | KindUnion
  | KindEnum
  | KindInputObject
  | KindList
  | KindNonNull
  | KindInputUnion
  | KindInterface
  deriving (Eq, Show, Lift)

instance RenderGQL TypeKind where
  renderGQL KindScalar = "SCALAR"
  renderGQL KindObject {} = "OBJECT"
  renderGQL KindUnion = "UNION"
  renderGQL KindInputUnion = "INPUT_OBJECT"
  renderGQL KindEnum = "ENUM"
  renderGQL KindInputObject = "INPUT_OBJECT"
  renderGQL KindList = "LIST"
  renderGQL KindNonNull = "NON_NULL"
  renderGQL KindInterface = "INTERFACE"

--  Definitions:
--     Strictness:
--        Strict: Value (Strict) Types.
--             members: {scalar, enum , input}
--        Lazy: Resolver (lazy) Types
--             members: strict + {object, interface, union}
class Strictness t where
  isResolverType :: t -> Bool

instance Strictness TypeKind where
  isResolverType (KindObject _) = True
  isResolverType KindUnion = True
  isResolverType KindInterface = True
  isResolverType _ = False

mkBaseType :: TypeWrapper
mkBaseType = BaseType True

mkMaybeType :: TypeWrapper
mkMaybeType = BaseType False

-- TypeWrappers
-----------------------------------------------------------------------------------
data TypeWrapper
  = TypeList Bool TypeWrapper
  | BaseType Bool
  deriving (Show, Eq, Lift)

data DataTypeWrapper
  = ListType DataTypeWrapper Bool
  | UnwrappedType TypeName Bool
  deriving (Show, Lift)

isStronger :: TypeRef -> TypeRef -> Bool
isStronger t1 t2 = typeConName t1 == typeConName t2 && isStronger' (typeWrappers t1) (typeWrappers t2)

isStronger' :: TypeWrapper -> TypeWrapper -> Bool
isStronger' (TypeList nonNull1 x1) (TypeList nonNull2 x2) = nonNull1 >= nonNull2 && isStronger' x1 x2
isStronger' (BaseType x) (BaseType y) = x >= y
isStronger' x y = x == y

toGQLWrapper :: TypeWrapper -> TypeName -> DataTypeWrapper
toGQLWrapper (TypeList nonNull tw) name = ListType (toGQLWrapper tw name) nonNull
toGQLWrapper (BaseType nonNull) name = UnwrappedType name nonNull

toTypeRef :: DataTypeWrapper -> TypeRef
toTypeRef (UnwrappedType typename nonNull) = TypeRef typename (BaseType nonNull)
toTypeRef (ListType wrapper nonNull) =
  let TypeRef typename hsWrappers = toTypeRef wrapper
   in TypeRef typename (TypeList nonNull hsWrappers)

-- TypeRef
-------------------------------------------------------------------
data TypeRef = TypeRef
  { typeConName :: TypeName,
    typeWrappers :: TypeWrapper
  }
  deriving (Show, Eq, Lift)

mkTypeRef :: TypeName -> TypeRef
mkTypeRef typeConName = TypeRef {typeConName, typeWrappers = mkBaseType}

instance RenderGQL TypeRef where
  renderGQL TypeRef {typeConName, typeWrappers} = showGQLWrapper (toGQLWrapper typeWrappers typeConName)
    where
      showGQLWrapper (ListType xs isNonNull) = "[" <> showGQLWrapper xs <> "]" <> renderNonNull isNonNull
      showGQLWrapper (UnwrappedType name isNonNull) = renderGQL name <> renderNonNull isNonNull
      renderNonNull True = "!"
      renderNonNull False = ""

instance Msg TypeRef where
  msg = msg . FieldName . LT.toStrict . decodeUtf8 . render

class Nullable a where
  isNullable :: a -> Bool
  toNullable :: a -> a

instance Nullable TypeWrapper where
  isNullable (TypeList nonNull _) = not nonNull
  isNullable (BaseType nonNull) = not nonNull

instance Nullable TypeRef where
  isNullable = isNullable . typeWrappers
  toNullable TypeRef {..} = TypeRef {typeWrappers = toNullable typeWrappers, ..}
