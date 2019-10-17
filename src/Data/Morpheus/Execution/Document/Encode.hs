{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Execution.Document.Encode
  ( deriveEncode
  ) where

import           Data.Text                             (unpack)
import           Data.Typeable                         (Typeable)
import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Server.Encode (Encode (..), ObjectResolvers (..))
import           Data.Morpheus.Types.GQLType           (TRUE)
import           Data.Morpheus.Types.Internal.Data     (DataField (..), SUBSCRIPTION, isSubscription)
import           Data.Morpheus.Types.Internal.DataD    (ConsD (..), GQLTypeD (..), TypeD (..))
import           Data.Morpheus.Types.Internal.Resolver (GADTResolver, PureOperation, SubResolver)
import           Data.Morpheus.Types.Internal.TH       (applyT, destructRecord, instanceHeadMultiT, typeT)

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
deriveEncode :: GQLTypeD -> Q [Dec]
deriveEncode GQLTypeD {typeKindD, typeD = TypeD {tName, tCons = [ConsD {cFields}]}} =
  pure <$> instanceD (cxt constrains) appHead methods
  where
    instanceArgs
          | isSubscription typeKindD = conT ''SUBSCRIPTION :map (varT . mkName) ["m","e"]
          | otherwise =  map (varT . mkName) ["o","m","e"]
    mainType = applyT (mkName tName) [mainTypeArg] -- defines  (<Type> (SubResolver m e)) or (<Type> (Resolver m))
      where
        mainTypeArg
          | isSubscription typeKindD = typeT ''SubResolver ["m", "e"] -- (SubResolver m e)
          | otherwise = typeT ''GADTResolver ["fieldOKind","m","e"] -- (Resolver m)
    -----------------------------------------------------------------------------------------
    typeables
         | isSubscription typeKindD =  []
         | otherwise = [typeT ''PureOperation ["fieldOKind"], typeT ''Typeable ["fieldOKind"] , typeT ''Typeable ["o"]]
    -- defines Constraint: (Typeable m, Monad m)
    constrains = typeables <>[typeT ''Monad ["m"], applyT ''Encode (mainType:instanceArgs) , typeT ''Typeable ["m"],typeT ''Typeable ["e"]]
    -------------------------------------------------------------------
    -- defines: instance <constraint> =>  ObjectResolvers ('TRUE) (<Type> (ResolveT m)) (ResolveT m value) where
    appHead = instanceHeadMultiT ''ObjectResolvers (conT ''TRUE) (mainType: instanceArgs)
    ------------------------------------------------------------------
    -- defines: objectResolvers <Type field1 field2 ...> = [("field1",encode field1),("field2",encode field2), ...]
    methods = [funD 'objectResolvers [clause argsE (normalB body) []]]
      where
        argsE = [varP (mkName "_"), destructRecord tName varNames]
        body = listE $ map decodeVar varNames
        decodeVar name = [|(name, encode $(varName))|]
          where
            varName = varE $ mkName name
        varNames = map (unpack . fieldName) cFields
deriveEncode _ = pure []
