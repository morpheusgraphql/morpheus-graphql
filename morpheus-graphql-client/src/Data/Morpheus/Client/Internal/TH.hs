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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.TH
  ( deriveIfNotDefined,
    declareIfNotDeclared,
    toJSONObjectMethod,
    fromJSONUnionMethod,
    fromJSONObjectMethod,
    ValueMatch,
    MValue (..),
    printMatch,
  )
where

import Data.Aeson
  ( withObject,
    (.:),
    (.=),
  )
import Data.Aeson.Types ((.:?))
import Data.Foldable (foldr1)
import Data.Morpheus.Client.Internal.Utils
  ( invalidConstructorError,
    omitNulls,
    takeValueType,
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenField (..),
    CodeGenType (..),
    CodeGenTypeName (..),
    TypeClassInstance (..),
    getFullName,
  )
import Data.Morpheus.CodeGen.TH
  ( ToString,
    toCon,
    toName,
    toString,
    toVar,
    v',
    _',
  )
import Data.Morpheus.CodeGen.Utils
  ( camelCaseFieldName,
  )
import Data.Morpheus.Types.Internal.AST (TypeName)
import Language.Haskell.TH
import Relude hiding (ToString, toString)

failExp :: ExpQ
failExp = appE (varE 'invalidConstructorError) v'

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

hasInstance :: Name -> CodeGenTypeName -> Q Bool
hasInstance typeClass tName = isInstance typeClass [ConT (toName tName)]

deriveIfNotDefined :: (TypeClassInstance a -> Q Dec) -> TypeClassInstance a -> Q [Dec]
deriveIfNotDefined derivation dec = do
  exists <- isTypeDeclared (typeClassTarget dec)
  if exists
    then do
      has <- hasInstance (typeClassName dec) (typeClassTarget dec)
      if has
        then pure []
        else mkDerivation
    else mkDerivation
  where
    mkDerivation :: Q [Dec]
    mkDerivation = pure <$> derivation dec

declareIfNotDeclared :: (CodeGenType -> Q a) -> CodeGenType -> Q [a]
declareIfNotDeclared f c = do
  exists <- isTypeDeclared (cgTypeName c)
  if exists
    then pure []
    else pure <$> f c

originalLit :: ToString TypeName a => CodeGenTypeName -> Q a
originalLit = toString . typename

-- EXPORTS

toJSONObjectMethod :: CodeGenConstructor -> ExpQ
toJSONObjectMethod CodeGenConstructor {..} = pure $ AppE (VarE 'omitNulls) (mkFieldsE constructorName '(.=) constructorFields)

fromJSONUnionMethod :: CodeGenType -> ExpQ
fromJSONUnionMethod CodeGenType {..} = appE (toVar 'takeValueType) (matchWith elseCondition f cgConstructors)
  where
    elseCondition =
      (tupP [_', v'],) . decodeObjectE
        <$> find ((typename cgTypeName ==) . typename . constructorName) cgConstructors
    f cons@CodeGenConstructor {..} =
      ( tupP [originalLit constructorName, if null constructorFields then _' else v'],
        decodeObjectE cons
      )

fromJSONObjectMethod :: CodeGenConstructor -> ExpQ
fromJSONObjectMethod con@CodeGenConstructor {constructorName} = withBody <$> decodeObjectE con
  where
    withBody body = AppE (AppE (VarE 'withObject) name) (LamE [v'] body)
    name :: Exp
    name = toString (getFullName constructorName)

type ValueMatch = [MValue]

data MValue
  = MFrom TypeName TypeName
  | MTo TypeName TypeName
  | MFunction String Name

printMatch :: ValueMatch -> ExpQ
printMatch = lamCaseE . map buildMatch
  where
    buildMatch (MFrom a b) = match (toString a) (normalB (appE (toVar 'pure) (toCon b))) []
    buildMatch (MTo a b) = match (toCon a) (normalB (toString b)) []
    buildMatch (MFunction v name) = match (toVar v) (normalB $ appE (varE name) v') []

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
