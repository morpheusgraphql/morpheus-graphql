{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Type
  ( TypeRef (..),
    TypeWrapper (..),
    Nullable (..),
    Strictness (..),
    TypeKind (..),
    isStronger,
    mkTypeRef,
    mkBaseType,
    mkMaybeType,
  )
where

import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    Rendering,
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
  = TypeList {listElementType :: TypeWrapper, isRequiredList :: Bool}
  | BaseType Bool
  deriving (Show, Eq, Lift)

mkBaseType :: TypeWrapper
mkBaseType = BaseType True

mkMaybeType :: TypeWrapper
mkMaybeType = BaseType False

isStrongerWrapper :: TypeWrapper -> TypeWrapper -> Bool
isStrongerWrapper (TypeList x1 nonNull1) (TypeList x2 nonNull2) = nonNull1 >= nonNull2 && isStrongerWrapper x1 x2
isStrongerWrapper (BaseType x) (BaseType y) = x >= y
isStrongerWrapper x y = x == y

-- TypeRef
-------------------------------------------------------------------
data TypeRef = TypeRef
  { typeConName :: TypeName,
    typeWrappers :: TypeWrapper
  }
  deriving (Show, Eq, Lift)

mkTypeRef :: TypeName -> TypeRef
mkTypeRef typeConName = TypeRef {typeConName, typeWrappers = mkBaseType}

isStronger :: TypeRef -> TypeRef -> Bool
isStronger t1 t2 = typeConName t1 == typeConName t2 && isStrongerWrapper (typeWrappers t1) (typeWrappers t2)

instance RenderGQL TypeRef where
  renderGQL TypeRef {typeConName, typeWrappers} = renderWrapper typeWrappers
    where
      renderWrapper (TypeList xs isNonNull) = "[" <> renderWrapper xs <> "]" <> renderNonNull isNonNull
      renderWrapper (BaseType isNonNull) = renderGQL typeConName <> renderNonNull isNonNull

renderNonNull :: Bool -> Rendering
renderNonNull True = "!"
renderNonNull False = ""

instance Msg TypeRef where
  msg = msg . FieldName . LT.toStrict . decodeUtf8 . render

class Nullable a where
  isNullable :: a -> Bool
  toNullable :: a -> a

instance Nullable TypeWrapper where
  isNullable (TypeList _ nonNull) = not nonNull
  isNullable (BaseType nonNull) = not nonNull
  toNullable (TypeList t _) = TypeList t False
  toNullable BaseType {} = BaseType False

instance Nullable TypeRef where
  isNullable = isNullable . typeWrappers
  toNullable TypeRef {..} = TypeRef {typeWrappers = toNullable typeWrappers, ..}
