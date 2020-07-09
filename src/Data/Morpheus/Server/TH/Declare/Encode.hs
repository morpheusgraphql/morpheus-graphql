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
  ( _',
    apply,
    destructRecord,
    e',
    funDSimple,
    m',
    mkFieldsE,
    o',
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

vars :: [Type]
vars = [o', e', m']

mkType :: TypeName -> Type
mkType tName = mainType
  where
    mainTypeArg = [apply ''Resolver vars]
    mainType = apply tName mainTypeArg

genHeadType :: TypeName -> [Type]
genHeadType tName = mkType tName : vars

-- defines Constraint: (Monad m, ...)
mkConstrains :: TypeName -> [Type]
mkConstrains tName =
  [ apply ''Monad [m'],
    apply ''Encode (genHeadType tName),
    apply ''LiftOperation [o']
  ]
    <> map constraintTypeable [o', e', m']

mkObjectE :: TypeName -> Exp -> Exp
mkObjectE name = AppE (AppE (VarE 'mkObject) (typeNameStringE name))

mkEntry ::
  Encode resolver o e m =>
  a ->
  resolver ->
  (a, Resolver o e m (ResModel o e m))
mkEntry name field = (name, encode field)

decodeObjectE :: TypeName -> [FieldDefinition cat s] -> Exp
decodeObjectE tName cFields =
  withPure $
    mkObjectE
      tName
      (mkFieldsE 'mkEntry cFields)

-- | defines: ObjectResolvers ('TRUE) (<Type> (ResolveT m)) (ResolveT m value)
instanceType :: TypeName -> Type
instanceType tName = apply ''ExploreResolvers (ConT ''TRUE : genHeadType tName)

-- | defines: objectResolvers <Type field1 field2 ...> = [("field1",encode field1),("field2",encode field2), ...]
exploreResolversD :: TypeName -> [FieldDefinition cat s] -> DecQ
exploreResolversD tName fields = funDSimple 'exploreResolvers args body
  where
    args = [_', destructRecord tName fields]
    body = pure (decodeObjectE tName fields)

deriveEncode :: ServerTypeDefinition cat s -> Q [Dec]
deriveEncode ServerTypeDefinition {tName, tCons = [ConsD {cFields}]} =
  pure <$> instanceD context typeDef funDefs
  where
    context = cxt (map pure $ mkConstrains tName)
    typeDef = pure (instanceType tName)
    funDefs = [exploreResolversD tName cFields]
deriveEncode _ = pure []
