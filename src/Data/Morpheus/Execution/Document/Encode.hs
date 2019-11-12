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
                                                )
import           Data.Morpheus.Types.Internal.AST.DataD
                                                ( ConsD(..)
                                                , GQLTypeD(..)
                                                , TypeD(..)
                                                )
import           Data.Morpheus.Types.Internal.Resolver
                                                ( Resolver
                                                , MapGraphQLT(..)
                                                , Resolving
                                                , PureOperation
                                                )
import           Data.Morpheus.Types.Internal.TH
                                                ( applyT
                                                , destructRecord
                                                , instanceHeadMultiT
                                                , typeT
                                                )

-- @Subscription:
--
--     instance (Monad m, Typeable m) => ObjectResolvers 'True (<Subscription> (SubResolver m e c)) (SubResolveT m e c Value) where
--          objectResolvers _ (<Subscription> x y) = [("newAddress", encode x), ("newUser", encode y)]
--
-- @Object:
--
--   instance (Monad m, Typeable m) => ObjectResolvers 'True (<Object> (Resolver m)) (ResolveT m Value) where
--          objectResolvers _ (<Object> x y) = [("field1", encode x), ("field2", encode y)]
--
--

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
                | otherwise = typeT ''Resolver ("fieldOKind" : encodeVars)
  -----------------------------------------------------------------------------------------
  typeables
    | isSubscription typeKindD
    = [ applyT ''MapGraphQLT $ map conT [''QUERY, ''SUBSCRIPTION]
      , applyT ''Resolving ([conT ''QUERY] <> encodeVarsT)
      ]
    | otherwise
    = [ typeT ''PureOperation ["fieldOKind"]
      , typeT ''MapGraphQLT   ["fieldOKind", "o"]
      , typeT ''Resolving     ("fieldOKind" : encodeVars)
      , typeT ''Typeable      ["fieldOKind"]
      , typeT ''Typeable      ["o"]
      ]
  -- defines Constraint: (Typeable m, Monad m)
  constrains =
    typeables
      <> [ typeT ''Monad ["m"]
         , applyT ''Encode (mainType : instanceArgs)
         , typeT ''Typeable ["e"]
         , typeT ''Typeable ["m"]
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
