{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.TH
  ( matchWith,
    decodeObjectE,
    mkFieldsE,
    destructRecord,
  )
where

import Control.Applicative ((<*>), pure)
import Control.Monad.Fail (fail)
import Data.Foldable (foldl, foldr1)
import Data.Functor ((<$>))
import Data.Maybe (Maybe (..))
import Data.Morpheus.Internal.TH (toCon, toString, toVarE, v')
import Data.Morpheus.Internal.Utils
  ( capitalize,
    nameSpaceField,
    nameSpaceType,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldDefinition (..),
    FieldName (..),
    TypeKind (..),
    TypeName (..),
    TypeRef (..),
    TypeWrapper (..),
    convertToHaskellName,
    isEnum,
    isNullable,
    isOutputObject,
    readName,
  )
import Data.Semigroup ((<>))
import Data.Text (unpack)
import Language.Haskell.TH
import Prelude
  ( ($),
    (.),
    (==),
    Bool (..),
    String,
    id,
    map,
    otherwise,
    show,
    (||),
  )

matchWith ::
  Bool ->
  (t -> (PatQ, ExpQ)) ->
  [t] ->
  ExpQ
matchWith isClosed f xs = lamCaseE (map buildMatch xs <> fallback)
  where
    fallback
      | isClosed = []
      | otherwise = [elseCaseEXP]
    buildMatch x = match pat (normalB body) []
      where
        (pat, body) = f x

elseCaseEXP :: MatchQ
elseCaseEXP = match v' body []
  where
    body =
      normalB $
        appE
          (toVarE 'fail)
          ( uInfixE
              (appE (varE 'show) v')
              (varE '(<>))
              (stringE " is Not Valid Union Constructor")
          )

decodeObjectE :: (Bool -> Name) -> TypeName -> [FieldDefinition cat s] -> ExpQ
decodeObjectE _ conName [] = appE (varE 'pure) (toCon conName)
decodeObjectE funName conName fields =
  uInfixE
    (toCon conName)
    (varE '(<$>))
    (foldr1 withApplicative $ map (defField funName) fields)

withApplicative :: ExpQ -> ExpQ -> ExpQ
withApplicative x = uInfixE x (varE '(<*>))

defField :: (Bool -> Name) -> FieldDefinition cat s -> ExpQ
defField f field@FieldDefinition {fieldName} = uInfixE v' (varE $ f (isNullable field)) (toString fieldName)

-- | 'mkFieldsE'
--
--  input :
--  >>>
--       mkFieldsE 'mkValue [FieldDefinition { fieldName = \"field1" ,..} ,..]
--  >>>
--
--  expression :
--  >>>
--    [ mkValue \"field1\" field1,
--    ..
--    ]
-- >>>
mkFieldsE :: Name -> [FieldDefinition cat s] -> Exp
mkFieldsE name = ListE . map (mkEntryWith name)

--  input : mkFieldWith 'mkValue (FieldDefinition { fieldName = "field1", ..})
--  expression: mkValue "field1"  field1
mkEntryWith ::
  Name ->
  FieldDefinition cat s ->
  Exp
mkEntryWith f FieldDefinition {fieldName} =
  AppE
    (AppE (VarE f) (toString fieldName))
    (toVar fieldName)

-- |
-- input:
-- >>>
-- destructRecord "User" ["name","id"]
-- >>>
--
-- expression:
-- >>>
-- (User name id)
-- >>>
destructRecord :: TypeName -> [FieldDefinition cat s] -> PatQ
destructRecord conName fields = conP (toName conName) (vars names)
  where
    names = map fieldName fields
