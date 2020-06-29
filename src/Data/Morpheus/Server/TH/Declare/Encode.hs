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
    mkTypeName,
    nameStringE,
    nameVarE,
    nameVarP,
    nameVarT,
    typeT,
  )
import Data.Morpheus.Server.Deriving.Encode
  ( Encode (..),
    ExploreResolvers (..),
  )
import Data.Morpheus.Server.Internal.TH.Types (ServerTypeDefinition (..))
import Data.Morpheus.Types.Internal.AST
  ( ConsD (..),
    FieldDefinition (..),
    TRUE,
    TypeName (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( LiftOperation,
    ObjectResModel (..),
    ResModel (..),
    Resolver,
  )
import Data.Semigroup ((<>))
import Data.Typeable (Typeable)
import Language.Haskell.TH

m_ :: TypeName
m_ = "m"

po_ :: TypeName
po_ = "parentOparation"

e_ :: TypeName
e_ = "encodeEvent"

encodeVars :: [TypeName]
encodeVars = [e_, m_]

deriveEncode :: ServerTypeDefinition cat -> Q [Dec]
deriveEncode ServerTypeDefinition {tName, tCons = [ConsD {cFields}]} =
  pure <$> instanceD (cxt constrains) appHead methods
  where
    instanceArgs = map nameVarT (po_ : encodeVars)
    mainType = applyT (mkTypeName tName) [mainTypeArg]
      where
        mainTypeArg = typeT ''Resolver (po_ : encodeVars)
    -------------------------
    iLiftOp op = applyT ''LiftOperation [nameVarT op]
    -------------------------
    iTypeable name = typeT ''Typeable [name]
    -------------------------------------------
    -- defines Constraint: (Typeable m, Monad m)
    constrains =
      [ typeT ''Monad [m_],
        applyT ''Encode (mainType : instanceArgs),
        iTypeable e_,
        iTypeable m_,
        iLiftOp po_,
        iTypeable po_
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
    methods = [funD 'exploreResolvers [clause argsE (normalB body) []]]
      where
        argsE = [nameVarP "_", destructRecord tName varNames]
        body =
          appE (varE 'pure)
            $ appE
              (conE 'ResObject)
            $ appE
              ( appE
                  (conE 'ObjectResModel)
                  (nameStringE tName)
              )
              (listE $ map decodeVar varNames)
        decodeVar name = [|(name, encode $(varName))|]
          where
            varName = nameVarE name
        varNames = map fieldName cFields
deriveEncode _ = pure []
