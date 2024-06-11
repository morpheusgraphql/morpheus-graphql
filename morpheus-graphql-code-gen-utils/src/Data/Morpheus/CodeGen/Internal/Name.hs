{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.CodeGen.Internal.Name
  ( toHaskellTypeName,
    camelCaseTypeName,
    toHaskellName,
    camelCaseFieldName,
  )
where

import Data.Char
  ( toLower,
    toUpper,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    TypeName,
    packName,
    unpackName,
  )
import qualified Data.Morpheus.Types.Internal.AST as N
import qualified Data.Text as T
import Relude hiding
  ( ToString (..),
    Type,
  )

mapFstChar :: (Char -> Char) -> Text -> Text
mapFstChar f x
  | T.null x = x
  | otherwise = T.singleton (f $ T.head x) <> T.tail x

capitalize :: Text -> Text
capitalize = mapFstChar toUpper

camelCaseTypeName :: [N.Name t] -> TypeName -> TypeName
camelCaseTypeName list name =
  packName
    $ T.concat
    $ map (capitalize . unpackName) (list <> [coerce name])

toHaskellTypeName :: TypeName -> Text
toHaskellTypeName "String" = "Text"
toHaskellTypeName "Boolean" = "Bool"
toHaskellTypeName "Float" = "Double"
toHaskellTypeName name
  | T.head (unpackName name) == '_' = capitalize $ T.tail (unpackName name)
  | otherwise = capitalize $ unpackName name
{-# INLINE toHaskellTypeName #-}

uncapitalize :: Text -> Text
uncapitalize = mapFstChar toLower

camelCaseFieldName :: TypeName -> FieldName -> FieldName
camelCaseFieldName nSpace name =
  packName
    $ uncapitalize (unpackName nSpace)
    <> capitalize (unpackName name)

toHaskellName :: FieldName -> String
toHaskellName name
  | isReserved name = T.unpack (unpackName name <> "'")
  | otherwise = T.unpack (uncapitalize (unpackName name))
{-# INLINE toHaskellName #-}

-- handle reserved Names
isReserved :: FieldName -> Bool
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
