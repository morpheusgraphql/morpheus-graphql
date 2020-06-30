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

m_ :: TypeName
m_ = "m"

po_ :: TypeName
po_ = "oparation"

e_ :: TypeName
e_ = "encodeEvent"

encodeVars :: [TypeName]
encodeVars = [po_, e_, m_]

deriveEncode :: ServerTypeDefinition cat -> Q [Dec]
deriveEncode ServerTypeDefinition {tName, tCons = [ConsD {cFields}]} =
  pure <$> instanceD (cxt constrains) appHead methods
  where
    instanceArgs = map nameVarT encodeVars
    mainTypeArg = [typeT ''Resolver encodeVars]
    mainType = applyT (mkTypeName tName) mainTypeArg
    -------------------------------------------
    -- defines Constraint: (Monad m, ...)
    constrains =
      [ typeT ''Monad [m_],
        applyT ''Encode (mainType : instanceArgs),
        applyT ''LiftOperation [nameVarT po_],
        constraintTypeable e_,
        constraintTypeable m_,
        constraintTypeable po_
      ]
    -------------------------------------------------------------------
    -- defines: instance <constraint> =>  ObjectResolvers ('TRUE) (<Type> (ResolveT m)) (ResolveT m value) where
    appHead =
      instanceHeadMultiT
        ''ExploreResolvers
        (conT ''TRUE)
        (mainType : instanceArgs)
    ------------------------------------------------------------------
    -- defines: objectResolvers <Type field1 field2 ...> = [("field1",encode field1),("field2",encode field2), ...]
    methods = [simpleFunD 'exploreResolvers args body]
      where
        args = [nameVarP "_", destructRecord tName cFields]
        body = pure (decodeObjectE tName cFields)
deriveEncode _ = pure []

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
