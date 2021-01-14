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
    isWeaker,
    mkTypeRef,
    toGQLWrapper,
    toHSWrappers,
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

isWeaker :: [TypeWrapper] -> [TypeWrapper] -> Bool
isWeaker (TypeMaybe : xs1) (TypeMaybe : xs2) = isWeaker xs1 xs2
isWeaker (TypeMaybe : _) _ = True
isWeaker (_ : xs1) (_ : xs2) = isWeaker xs1 xs2
isWeaker _ _ = False

toGQLWrapper :: [TypeWrapper] -> [DataTypeWrapper]
toGQLWrapper (TypeMaybe : (TypeMaybe : tw)) = toGQLWrapper (TypeMaybe : tw)
toGQLWrapper (TypeMaybe : (TypeList : tw)) = ListType : toGQLWrapper tw
toGQLWrapper (TypeList : tw) = [NonNullType, ListType] <> toGQLWrapper tw
toGQLWrapper [TypeMaybe] = []
toGQLWrapper [] = [NonNullType]

toHSWrappers :: [DataTypeWrapper] -> [TypeWrapper]
toHSWrappers (NonNullType : (NonNullType : xs)) =
  toHSWrappers (NonNullType : xs)
toHSWrappers (NonNullType : (ListType : xs)) = TypeList : toHSWrappers xs
toHSWrappers (ListType : xs) = [TypeMaybe, TypeList] <> toHSWrappers xs
toHSWrappers [] = [TypeMaybe]
toHSWrappers [NonNullType] = []

renderWrapped :: RenderGQL a => a -> [TypeWrapper] -> Rendering
renderWrapped x wrappers = showGQLWrapper (toGQLWrapper wrappers)
  where
    showGQLWrapper [] = renderGQL x
    showGQLWrapper (ListType : xs) = "[" <> showGQLWrapper xs <> "]"
    showGQLWrapper (NonNullType : xs) = showGQLWrapper xs <> "!"

data TypeWrapper
  = TypeList
  | TypeMaybe
  deriving (Show, Eq, Lift)

data DataTypeWrapper
  = ListType
  | NonNullType
  deriving (Show, Lift)

data TypeRef = TypeRef
  { typeConName :: TypeName,
    typeArgs :: Maybe String,
    typeWrappers :: [TypeWrapper]
  }
  deriving (Show, Eq, Lift)

mkTypeRef :: TypeName -> TypeRef
mkTypeRef typeConName =
  TypeRef {typeConName, typeWrappers = [], typeArgs = Nothing}

instance RenderGQL TypeRef where
  renderGQL TypeRef {typeConName, typeWrappers} = renderWrapped typeConName typeWrappers

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
