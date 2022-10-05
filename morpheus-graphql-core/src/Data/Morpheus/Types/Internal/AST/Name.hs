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
    unitTypeName,
    unitFieldName,
    isNotSystemTypeName,
    isNotSystemFieldName,
    intercalate,
    NAME (..),
    FragmentName,
    isValidName,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON (..),
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    fromText,
    renderGQL,
  )
import Data.Morpheus.Types.Internal.AST.Error
  ( Msg (..),
  )
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.Key as A
#endif
import qualified Data.Text as T
#if MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH
  ( Quote,
    Code,
    unsafeCodeCoerce,
    stringE
  )
import Language.Haskell.TH.Syntax(
    Lift(..)
  )
# else
import Language.Haskell.TH
  ( stringE,
  )
import Language.Haskell.TH.Syntax
  ( Lift (..),
    Q,
    TExp,
    unsafeTExpCoerce,
  )
#endif
import Data.Char (isLetter, isNumber)
import qualified Data.List as L
import qualified Language.Haskell.TH.Syntax as TH
import Relude hiding
  ( ByteString,
    decodeUtf8,
    intercalate,
  )

data NAME
  = TYPE
  | FIELD
  | FRAGMENT

newtype Name (t :: NAME) = Name {_unpackName :: Text}
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
  msg name = msg $ "\"" <> _unpackName name <> "\""

isValidName :: Name t -> Bool
isValidName n = T.all isStart (T.take 1 name) && T.all isContinue (T.drop 1 name)
  where
    name = unpackName n
    isStart c = c == '_' || isLetter c
    isContinue c = isStart c || isNumber c

class NamePacking a where
  packName :: a -> Name t
  unpackName :: Name t -> a

instance NamePacking TH.Name where
  packName (TH.Name name _) = Name $ T.pack (occName name)
    where
      occName (TH.OccName n) = takeWhile (/= ':') (removeSelector n)
      removeSelector x = fromMaybe x (L.stripPrefix "$sel:" x)
  unpackName = TH.mkName . toString . _unpackName

instance NamePacking Text where
  packName = Name
  unpackName = _unpackName

#if MIN_VERSION_aeson(2,0,0)
instance NamePacking Key where
  packName = Name . A.toText
  unpackName = A.fromText . _unpackName
#endif

instance Lift (Name t) where
  lift = stringE . T.unpack . unpackName

#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = liftTypedString . unpackName
    where
      liftTypedString :: (Quote m) => Text -> Code m (Name t)
      liftTypedString = unsafeCodeCoerce . stringE . T.unpack
      {-# INLINE liftTypedString #-}
#elif MIN_VERSION_template_haskell(2,16,0)
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
