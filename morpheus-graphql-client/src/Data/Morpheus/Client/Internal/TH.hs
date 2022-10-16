{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.TH
  ( matchWith,
    decodeObjectE,
    mkFieldsE,
    failExp,
    deriveIfNotDefined,
    declareIfNotDeclared,
    toJSONEnumMethod,
    originalLit,
    toJSONObjectMethod,
  )
where

import Data.Aeson ((.:), (.=))
import Data.Aeson.Types ((.:?))
import Data.Foldable (foldr1)
import Data.Morpheus.Client.Internal.Utils (omitNulls)
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenField (..),
    CodeGenType (cgTypeName),
    CodeGenTypeName (..),
    getFullName,
  )
import Data.Morpheus.CodeGen.TH
  ( ToString,
    toCon,
    toName,
    toString,
    toVar,
    v',
  )
import Data.Morpheus.CodeGen.Utils
  ( camelCaseFieldName,
  )
import Data.Morpheus.Types.Internal.AST (TypeName)
import Language.Haskell.TH
import Relude hiding (ToString, toString)

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

decodeObjectE :: CodeGenConstructor -> ExpQ
decodeObjectE CodeGenConstructor {..}
  | null constructorFields = appE [|pure|] (toCon constructorName)
  | otherwise =
    uInfixE
      (toCon constructorName)
      [|(<$>)|]
      (foldr1 withApplicative $ map defField constructorFields)

defField :: CodeGenField -> ExpQ
defField CodeGenField {..} = uInfixE v' (varE $ bindField fieldIsNullable) (toString fieldName)

bindField :: Bool -> Name
bindField nullable
  | nullable = '(.:?)
  | otherwise = '(.:)

withApplicative :: ExpQ -> ExpQ -> ExpQ
withApplicative x = uInfixE x [|(<*>)|]

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
mkFieldsE :: CodeGenTypeName -> Name -> [CodeGenField] -> Exp
mkFieldsE conName name = ListE . map (mkEntryWith conName name)

--  input : mkFieldWith 'mkValue (FieldDefinition { fieldName = "field1", ..})
--  expression: mkValue "field1"  field1
mkEntryWith ::
  CodeGenTypeName ->
  Name ->
  CodeGenField ->
  Exp
mkEntryWith conName f CodeGenField {fieldName} =
  AppE
    (AppE (VarE f) (toString fieldName))
    (toVar $ camelCaseFieldName (getFullName conName) fieldName)

isTypeDeclared :: CodeGenTypeName -> Q Bool
isTypeDeclared clientTypeName = do
  let name = toName clientTypeName
  m <- lookupTypeName (show name)
  case m of
    Nothing -> pure False
    _ -> pure True

hasInstance :: Name -> CodeGenType -> Q Bool
hasInstance typeClass clientDef = isInstance typeClass [ConT (toName (cgTypeName clientDef))]

deriveIfNotDefined :: (CodeGenType -> Q Dec) -> Name -> CodeGenType -> Q [Dec]
deriveIfNotDefined derivation typeClass clientDef = do
  exists <- isTypeDeclared (cgTypeName clientDef)
  if exists
    then do
      has <- hasInstance typeClass clientDef
      if has
        then pure []
        else mkDerivation
    else mkDerivation
  where
    mkDerivation :: Q [Dec]
    mkDerivation = pure <$> derivation clientDef

declareIfNotDeclared :: (CodeGenType -> Q a) -> CodeGenType -> Q [a]
declareIfNotDeclared f c = do
  exists <- isTypeDeclared (cgTypeName c)
  if exists
    then pure []
    else pure <$> f c

originalLit :: ToString TypeName a => CodeGenTypeName -> Q a
originalLit = toString . typename

-- EXPORTS

toJSONEnumMethod :: [CodeGenConstructor] -> ExpQ
toJSONEnumMethod = matchWith Nothing toJSONEnum

toJSONEnum :: CodeGenConstructor -> (PatQ, ExpQ)
toJSONEnum CodeGenConstructor {constructorName} = (toCon constructorName, originalLit constructorName)

toJSONObjectMethod :: CodeGenConstructor -> ExpQ
toJSONObjectMethod CodeGenConstructor {..} = pure $ AppE (VarE 'omitNulls) (mkFieldsE constructorName '(.=) constructorFields)
