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
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.TH
  ( matchWith,
    decodeObjectE,
    mkFieldsE,
    destructRecord,
    failExp,
    isTypeDeclared,
    hasInstance,
  )
where

import Data.Foldable (foldr1)
import Data.Morpheus.Client.Internal.Types (ClientTypeDefinition (..), TypeNameTH (..))
import Data.Morpheus.CodeGen.Internal.TH
  ( camelCaseFieldName,
    camelCaseTypeName,
    toCon,
    toName,
    toString,
    toVar,
    v',
    vars,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldDefinition (..),
    TypeName,
    isNullable,
  )
import Language.Haskell.TH
import Relude hiding (toString)

matchWith ::
  Maybe (PatQ, ExpQ) ->
  (t -> (PatQ, ExpQ)) ->
  [t] ->
  ExpQ
matchWith fbexp f xs = lamCaseE (map buildMatch xs <> fallback fbexp)
  where
    fallback (Just (pat, fb)) = [match pat (normalB fb) []]
    fallback _ = []
    buildMatch x = match pat (normalB body) []
      where
        (pat, body) = f x

failExp :: ExpQ
failExp =
  appE
    (toVar 'fail)
    ( uInfixE
        (appE [|show|] v')
        [|(<>)|]
        (stringE " is Not Valid Union Constructor")
    )

decodeObjectE :: (Bool -> Name) -> TypeName -> [FieldDefinition cat s] -> ExpQ
decodeObjectE _ conName [] = appE [|pure|] (toCon conName)
decodeObjectE funName conName fields =
  uInfixE
    (toCon conName)
    [|(<$>)|]
    (foldr1 withApplicative $ map (defField funName) fields)

withApplicative :: ExpQ -> ExpQ -> ExpQ
withApplicative x = uInfixE x [|(<*>)|]

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
mkFieldsE :: TypeName -> Name -> [FieldDefinition cat s] -> Exp
mkFieldsE conName name = ListE . map (mkEntryWith conName name)

--  input : mkFieldWith 'mkValue (FieldDefinition { fieldName = "field1", ..})
--  expression: mkValue "field1"  field1
mkEntryWith ::
  TypeName ->
  Name ->
  FieldDefinition cat s ->
  Exp
mkEntryWith conName f FieldDefinition {fieldName} =
  AppE
    (AppE (VarE f) (toString fieldName))
    (toVar $ camelCaseFieldName conName fieldName)

-- |
-- input:
-- >>>
-- WAS WAS destructRecord "User" ["name","id"]
-- >>>
--
-- expression:
-- >>>
-- WAS WAS (User name id)
-- >>>
destructRecord :: TypeName -> [FieldDefinition cat s] -> PatQ
destructRecord conName fields = conP (toName conName) (vars names)
  where
    names = map (camelCaseFieldName conName . fieldName) fields

isTypeDeclared :: ClientTypeDefinition -> Q Bool
isTypeDeclared clientDef = do
    let name = mkTypeName clientDef
    m <- lookupTypeName (show name)
    case m of
        Nothing -> pure False
        _ -> pure True

hasInstance :: Name -> ClientTypeDefinition -> Q Bool
hasInstance typeClass clientDef = do
    isInstance typeClass [ConT (mkTypeName clientDef)]

mkTypeName :: ClientTypeDefinition -> Name
mkTypeName ClientTypeDefinition{clientTypeName = TypeNameTH namespace typeName} =
    toType typeName
  where
    toType = toName . camelCaseTypeName namespace