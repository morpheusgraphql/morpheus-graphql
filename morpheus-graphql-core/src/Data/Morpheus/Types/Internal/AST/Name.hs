{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Name
  ( Name,
    packName,
    unpackName,
    FieldName,
    TypeName,
    fromHaskellName,
    toHaskellName,
    toHaskellTypeName,
    unitTypeName,
    unitFieldName,
    isNotSystemTypeName,
    isNotSystemFieldName,
    intercalate,
    camelCaseTypeName,
    camelCaseFieldName,
    NAME (..),
    FragmentName,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON (..),
  )
import Data.Char
  ( toLower,
    toUpper,
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    fromText,
    renderGQL,
  )
import Data.Morpheus.Types.Internal.AST.Error
  ( Msg (..),
  )
import qualified Data.Text as T
import Language.Haskell.TH
  ( stringE,
  )
import Language.Haskell.TH.Syntax
  ( Lift (..),
    Q,
    TExp,
    unsafeTExpCoerce,
  )
import Relude hiding
  ( ByteString,
    decodeUtf8,
    intercalate,
  )

data NAME
  = TYPE
  | FIELD
  | FRAGMENT

newtype Name (t :: NAME) = Name {unpackName :: Text}
  deriving
    (Generic)
  deriving newtype
    ( Show,
      Ord,
      Eq,
      IsString,
      ToString,
      Hashable,
      Semigroup,
      FromJSON,
      ToJSON
    )

instance Msg (Name t) where
  msg name = msg $ "\"" <> unpackName name <> "\""

packName :: Text -> Name t
packName = Name

instance Lift (Name t) where
  lift = stringE . T.unpack . unpackName

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedString . unpackName
    where
      liftTypedString :: IsString a => Text -> Q (TExp a)
      liftTypedString = unsafeTExpCoerce . stringE . T.unpack
      {-# INLINE liftTypedString #-}
#endif

instance RenderGQL (Name a) where
  renderGQL = fromText . unpackName

type FieldName = Name 'FIELD

type TypeName = Name 'TYPE

type FragmentName = Name 'FRAGMENT

intercalate :: Name t1 -> [Name t2] -> Name t3
intercalate (Name x) = Name . T.intercalate x . fmap unpackName
{-# INLINE intercalate #-}

-- handle reserved Names
isReserved :: Name t -> Bool
isReserved "case" = True
isReserved "class" = True
isReserved "data" = True
isReserved "default" = True
isReserved "deriving" = True
isReserved "do" = True
isReserved "else" = True
isReserved "foreign" = True
isReserved "if" = True
isReserved "import" = True
isReserved "in" = True
isReserved "infix" = True
isReserved "infixl" = True
isReserved "infixr" = True
isReserved "instance" = True
isReserved "let" = True
isReserved "module" = True
isReserved "newtype" = True
isReserved "of" = True
isReserved "then" = True
isReserved "type" = True
isReserved "where" = True
isReserved "_" = True
isReserved _ = False
{-# INLINE isReserved #-}

fromHaskellName :: String -> FieldName
fromHaskellName hsName
  | not (null hsName) && (T.last name == '\'') = Name (T.init name)
  | otherwise = Name name
  where
    name = T.pack hsName
{-# INLINE fromHaskellName #-}

toHaskellName :: FieldName -> String
toHaskellName name
  | isReserved name = T.unpack (unpackName name <> "'")
  | otherwise = T.unpack (uncapitalize (unpackName name))
{-# INLINE toHaskellName #-}

toHaskellTypeName :: TypeName -> String
toHaskellTypeName "String" = "Text"
toHaskellTypeName "Boolean" = "Bool"
toHaskellTypeName "Float" = "Double"
toHaskellTypeName (Name name) = T.unpack $ capitalize name
{-# INLINE toHaskellTypeName #-}

mapFstChar :: (Char -> Char) -> Text -> Text
mapFstChar f x
  | T.null x = x
  | otherwise = T.singleton (f $ T.head x) <> T.tail x

capitalize :: Text -> Text
capitalize = mapFstChar toUpper

uncapitalize :: Text -> Text
uncapitalize = mapFstChar toLower

camelCaseTypeName :: [Name t] -> TypeName -> TypeName
camelCaseTypeName list (Name name) =
  Name $ T.concat $
    map capitalize (map unpackName list <> [name])

camelCaseFieldName :: TypeName -> FieldName -> FieldName
camelCaseFieldName (Name nSpace) (Name name) = Name $ uncapitalize nSpace <> capitalize name

unitTypeName :: TypeName
unitTypeName = "Unit"
{-# INLINE unitTypeName #-}

unitFieldName :: FieldName
unitFieldName = "_"
{-# INLINE unitFieldName #-}

isNotSystemTypeName :: TypeName -> Bool
isNotSystemTypeName =
  ( `notElem`
      [ "__Schema",
        "__Type",
        "__Directive",
        "__TypeKind",
        "__Field",
        "__DirectiveLocation",
        "__InputValue",
        "__EnumValue",
        "String",
        "Float",
        "Int",
        "Boolean",
        "ID"
      ]
  )
{-# INLINE isNotSystemTypeName #-}

isNotSystemFieldName :: FieldName -> Bool
isNotSystemFieldName =
  ( `notElem`
      [ "__typename",
        "__schema",
        "__type"
      ]
  )
{-# INLINE isNotSystemFieldName #-}
