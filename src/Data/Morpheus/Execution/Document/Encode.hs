{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Execution.Document.Encode
  ( deriveEncode
  )
where

import           Data.Text                      ( unpack )
import           Data.Typeable                  ( Typeable )
import           Language.Haskell.TH
import           Data.Semigroup                 ( (<>) )

--
-- MORPHEUS
import           Data.Morpheus.Execution.Server.Encode
                                                ( Encode(..)
                                                , ObjectResolvers(..)
                                                )
import           Data.Morpheus.Types.GQLType    ( TRUE )
import           Data.Morpheus.Types.Internal.AST.Data
                                                ( DataField(..)
                                                , QUERY
                                                , SUBSCRIPTION
                                                , isSubscription
                                                , ConsD(..)
                                                , GQLTypeD(..)
                                                , TypeD(..)
                                                )
import           Data.Morpheus.Types.Internal.Resolver
                                                ( Resolver
                                                , MapStrategy(..)
                                                , LiftEither
                                                , ResolvingStrategy
                                                )
import           Data.Morpheus.Types.Internal.TH
                                                ( applyT
                                                , destructRecord
                                                , instanceHeadMultiT
                                                , typeT
                                                )

encodeVars :: [String]
encodeVars = ["e", "m"]

encodeVarsT :: [TypeQ]
encodeVarsT = map (varT . mkName) encodeVars

deriveEncode :: GQLTypeD -> Q [Dec]
deriveEncode GQLTypeD { typeKindD, typeD = TypeD { tName, tCons = [ConsD { cFields }] } }
  = pure <$> instanceD (cxt constrains) appHead methods
 where
  subARgs = conT ''SUBSCRIPTION : encodeVarsT
  instanceArgs | isSubscription typeKindD = subARgs
               | otherwise = map (varT . mkName) ("o" : encodeVars)
  mainType = applyT (mkName tName) [mainTypeArg]
   where
    mainTypeArg | isSubscription typeKindD = applyT ''Resolver subARgs
                | otherwise                = typeT ''Resolver (fo_ : encodeVars)
  -----------------------------------------------------------------------------------------
  fo_ = "fieldOperationKind"
  po_ = "o"
  ---------------------
  typeables
    | isSubscription typeKindD
    = [applyT ''MapStrategy $ map conT [''QUERY, ''SUBSCRIPTION]]
    | otherwise
    = [ iLiftEither ''ResolvingStrategy
      , iLiftEither ''Resolver
      , typeT ''MapStrategy [fo_, po_]
      , iTypeable fo_
      , iTypeable po_
      ]
  -------------------------
  iLiftEither name = applyT ''LiftEither [varT $ mkName fo_, conT name]
  -------------------------
  iTypeable name = typeT ''Typeable [name]
  -------------------------------------------
  -- defines Constraint: (Typeable m, Monad m)
  constrains =
    typeables
      <> [ typeT ''Monad ["m"]
         , applyT ''Encode (mainType : instanceArgs)
         , iTypeable "e"
         , iTypeable "m"
         ]
  -------------------------------------------------------------------
  -- defines: instance <constraint> =>  ObjectResolvers ('TRUE) (<Type> (ResolveT m)) (ResolveT m value) where
  appHead =
    instanceHeadMultiT ''ObjectResolvers (conT ''TRUE) (mainType : instanceArgs)
  ------------------------------------------------------------------
  -- defines: objectResolvers <Type field1 field2 ...> = [("field1",encode field1),("field2",encode field2), ...]
  methods = [funD 'objectResolvers [clause argsE (normalB body) []]]
   where
    argsE = [varP (mkName "_"), destructRecord tName varNames]
    body  = listE $ map decodeVar varNames
    decodeVar name = [|(name, encode $(varName))|]
      where varName = varE $ mkName name
    varNames = map (unpack . fieldName) cFields
deriveEncode _ = pure []
