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
    failExp,
  )
where

import Control.Applicative ((<*>), pure)
import Control.Monad.Fail (fail)
import Data.Foldable (foldr1)
import Data.Functor ((<$>))
import Data.Morpheus.Internal.TH
  ( _',
    toCon,
    toName,
    toString,
    toVar,
    toVarE,
    v',
    vars,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldDefinition (..),
    TypeName (..),
    isNullable,
  )
import Data.Semigroup ((<>))
import Language.Haskell.TH
import Prelude
  ( ($),
    (.),
    Bool (..),
    Maybe (..),
    map,
    show,
  )

matchWith ::
  Maybe ExpQ ->
  (t -> (PatQ, ExpQ)) ->
  [t] ->
  ExpQ
matchWith fbexp f xs = lamCaseE (map buildMatch xs <> fallback fbexp)
  where
    fallback (Just fb) = [match (tupP [_', v']) (normalB fb) []]
    fallback _ = []
    buildMatch x = match pat (normalB body) []
      where
        (pat, body) = f x

failExp :: ExpQ
failExp =
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
