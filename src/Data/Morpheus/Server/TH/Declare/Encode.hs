{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.TH.Declare.Encode
  ( deriveEncode,
  )
where

--
-- MORPHEUS

import Data.Morpheus.Internal.TH
  ( applyT,
    destructRecord,
    instanceHeadMultiT,
    m_,
    mkFieldsE,
    mkTypeName,
    nameVarP,
    nameVarT,
    simpleFunD,
    typeT,
  )
import Data.Morpheus.Server.Deriving.Encode
  ( Encode (..),
    ExploreResolvers (..),
  )
import Data.Morpheus.Server.Internal.TH.Types
  ( ServerTypeDefinition (..),
  )
import Data.Morpheus.Server.Internal.TH.Utils
  ( constraintTypeable,
    typeNameStringE,
    withPure,
  )
import Data.Morpheus.Types.Internal.AST
  ( ConsD (..),
    FieldDefinition (..),
    TRUE,
    TypeName (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( LiftOperation,
    ResModel,
    Resolver,
    mkObject,
  )
import Data.Semigroup ((<>))
import Language.Haskell.TH

o_ :: TypeName
o_ = "oparation"

e_ :: TypeName
e_ = "encodeEvent"

encodeVars :: [TypeName]
encodeVars = [o_, e_, m_]

genHeadType :: TypeName -> [Q Type]
genHeadType tName = mainType : instanceArgs
  where
    instanceArgs = map nameVarT encodeVars
    mainTypeArg = [typeT ''Resolver encodeVars]
    mainType = applyT (mkTypeName tName) mainTypeArg

-- defines Constraint: (Monad m, ...)
mkConstrains :: TypeName -> [Q Type]
mkConstrains tName =
  [ typeT ''Monad [m_],
    applyT ''Encode (genHeadType tName),
    applyT ''LiftOperation [nameVarT o_],
    constraintTypeable e_,
    constraintTypeable m_,
    constraintTypeable o_
  ]

mkObjectE :: TypeName -> Exp -> Exp
mkObjectE name = AppE (AppE (VarE 'mkObject) (typeNameStringE name))

mkEntry ::
  Encode resolver o e m =>
  a ->
  resolver ->
  (a, Resolver o e m (ResModel o e m))
mkEntry name field = (name, encode field)

decodeObjectE :: TypeName -> [FieldDefinition cat] -> Exp
decodeObjectE tName cFields =
  withPure $
    mkObjectE
      tName
      (mkFieldsE 'mkEntry cFields)

-- | defines: ObjectResolvers ('TRUE) (<Type> (ResolveT m)) (ResolveT m value)
instanceType :: TypeName -> Q Type
instanceType tName =
  instanceHeadMultiT
    ''ExploreResolvers
    (conT ''TRUE)
    (genHeadType tName)

-- | defines: objectResolvers <Type field1 field2 ...> = [("field1",encode field1),("field2",encode field2), ...]
exploreResolversD :: TypeName -> [FieldDefinition cat] -> DecQ
exploreResolversD tName fields = simpleFunD 'exploreResolvers args body
  where
    args = [nameVarP "_", destructRecord tName fields]
    body = pure (decodeObjectE tName fields)

deriveEncode :: ServerTypeDefinition cat -> Q [Dec]
deriveEncode ServerTypeDefinition {tName, tCons = [ConsD {cFields}]} =
  pure <$> instanceD context typeDef funDefs
  where
    context = cxt (mkConstrains tName)
    typeDef = instanceType tName
    funDefs = [exploreResolversD tName cFields]
deriveEncode _ = pure []
