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
    isWeaker,
    mkTypeRef,
    toGQLWrapper,
    toTypeRef,
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

-- TypeWrappers
-----------------------------------------------------------------------------------
data TypeWrapper
  = TypeList
  | TypeMaybe
  deriving (Show, Eq, Lift)

data DataTypeWrapper
  = ListType DataTypeWrapper Bool
  | UnwrappedType TypeName Bool
  deriving (Show, Lift)

isWeaker :: [TypeWrapper] -> [TypeWrapper] -> Bool
isWeaker (TypeMaybe : xs1) (TypeMaybe : xs2) = isWeaker xs1 xs2
isWeaker (TypeMaybe : _) _ = True
isWeaker (_ : xs1) (_ : xs2) = isWeaker xs1 xs2
isWeaker _ _ = False

toGQLWrapper :: [TypeWrapper] -> TypeName -> DataTypeWrapper
toGQLWrapper (TypeMaybe : (TypeMaybe : tw)) name = toGQLWrapper (TypeMaybe : tw) name
toGQLWrapper (TypeMaybe : (TypeList : tw)) name = ListType (toGQLWrapper tw name) False
toGQLWrapper (TypeList : tw) name = ListType (toGQLWrapper tw name) True
toGQLWrapper [TypeMaybe] name = UnwrappedType name False
toGQLWrapper [] name = UnwrappedType name True

toTypeRef :: DataTypeWrapper -> TypeRef
toTypeRef (UnwrappedType typename isNonNull)
  | isNonNull = TypeRef typename []
  | otherwise = TypeRef typename [TypeMaybe]
toTypeRef (ListType wrapper isNonNull) =
  let TypeRef typename hsWrappers = toTypeRef wrapper
      fullHsWrappers = if isNonNull then TypeList : hsWrappers else TypeMaybe : TypeList : hsWrappers
   in TypeRef typename fullHsWrappers

-- TypeRef
-------------------------------------------------------------------
data TypeRef = TypeRef
  { typeConName :: TypeName,
    typeWrappers :: [TypeWrapper]
  }
  deriving (Show, Eq, Lift)

mkTypeRef :: TypeName -> TypeRef
mkTypeRef typeConName = TypeRef {typeConName, typeWrappers = []}

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

instance Nullable [TypeWrapper] where
  isNullable (TypeMaybe : _) = True
  isNullable _ = False
  toNullable (TypeMaybe : xs) = TypeMaybe : xs
  toNullable xs = TypeMaybe : xs

instance Nullable TypeRef where
  isNullable = isNullable . typeWrappers
  toNullable TypeRef {..} = TypeRef {typeWrappers = toNullable typeWrappers, ..}
