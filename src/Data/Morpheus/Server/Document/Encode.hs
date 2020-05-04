{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.Document.Encode
  ( deriveEncode,
  )
where

--
-- MORPHEUS
import Data.Morpheus.Server.Deriving.Encode
  ( Encode (..),
    ExploreResolvers (..),
  )
import Data.Morpheus.Types.GQLType (TRUE)
import Data.Morpheus.Types.Internal.AST
  ( ConsD (..),
    FieldDefinition (..),
    GQLTypeD (..),
    Key,
    QUERY,
    SUBSCRIPTION,
    TypeD (..),
    isSubscription,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Deriving (..),
    LiftOperation,
    MapStrategy (..),
    ObjectDeriving (..),
    Resolver,
  )
import Data.Morpheus.Types.Internal.TH
  ( applyT,
    destructRecord,
    instanceHeadMultiT,
    typeT,
  )
import Data.Semigroup ((<>))
import Data.Text (unpack)
import Data.Typeable (Typeable)
import Language.Haskell.TH

m_ :: Key
m_ = "m"

fo_ :: Key
fo_ = "fieldOperationKind"

po_ :: Key
po_ = "parentOparation"

e_ :: Key
e_ = "encodeEvent"

encodeVars :: [Key]
encodeVars = [e_, m_]

encodeVarsT :: [TypeQ]
encodeVarsT = map (varT . mkName . unpack) encodeVars

deriveEncode :: GQLTypeD -> Q [Dec]
deriveEncode GQLTypeD {typeKindD, typeD = TypeD {tName, tCons = [ConsD {cFields}]}} =
  pure <$> instanceD (cxt constrains) appHead methods
  where
    subARgs = conT ''SUBSCRIPTION : encodeVarsT
    instanceArgs
      | isSubscription typeKindD = subARgs
      | otherwise = map (varT . mkName . unpack) (po_ : encodeVars)
    mainType = applyT (mkName $ unpack tName) [mainTypeArg]
      where
        mainTypeArg
          | isSubscription typeKindD = applyT ''Resolver subARgs
          | otherwise = typeT ''Resolver (fo_ : encodeVars)
    -----------------------------------------------------------------------------------------
    typeables
      | isSubscription typeKindD =
        [applyT ''MapStrategy $ map conT [''QUERY, ''SUBSCRIPTION]]
      | otherwise =
        [ iLiftOp po_,
          iLiftOp fo_,
          typeT ''MapStrategy [fo_, po_],
          iTypeable fo_,
          iTypeable po_
        ]
    -------------------------
    iLiftOp op = applyT ''LiftOperation [varT $ mkName $ unpack op]
    -------------------------
    iTypeable name = typeT ''Typeable [name]
    -------------------------------------------
    -- defines Constraint: (Typeable m, Monad m)
    constrains =
      typeables
        <> [ typeT ''Monad [m_],
             applyT ''Encode (mainType : instanceArgs),
             iTypeable e_,
             iTypeable m_
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
        argsE = [varP (mkName "_"), destructRecord tName varNames]
        body =
          appE (varE 'pure)
            $ appE
              (conE 'DerivingObject)
            $ appE
              ( appE
                  (conE 'ObjectDeriving)
                  (stringE (unpack tName))
              )
              (listE $ map (decodeVar . unpack) varNames)
        decodeVar name = [|(name, encode $(varName))|]
          where
            varName = varE $ mkName name
        varNames = map fieldName cFields
deriveEncode _ = pure []
