{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications      #-}

module Data.Morpheus.Execution.Internal.Declare
  ( declareType
  , declareResolverType
  ) where

import           Language.Haskell.TH

import           Data.Morpheus.Types.Internal.Data  (DataTypeKind (..))

--
-- MORPHEUS
import           Data.Morpheus.Types.Internal.DataD (AppD (..), ConsD (..), FieldD (..), KindD (..), ResolverKind (..),
                                                     TypeD (..), unKindD)
import           GHC.Generics                       (Generic)

type FUNC = (->)

declareType :: [Name] -> TypeD -> Dec
declareType = __declareType False Nothing

declareResolverType :: KindD -> [Name] -> TypeD -> Dec
declareResolverType x = __declareType False (Just x)

--
--
__declareType :: Bool -> Maybe KindD -> [Name] -> TypeD -> Dec
__declareType namespace kindD derivingList TypeD {tName, tCons} =
  DataD [] (mkName tName) tVars Nothing (map cons tCons) $ map derive (''Generic : derivingList)
  where
    gqlKind = unKindD <$> kindD
    isSubscription = kindD == Just SubscriptionD
    withTyCon = gqlKind == Just KindObject || gqlKind == Just KindUnion
    tVars
      | isSubscription = declareTyVar ["subscriptionM", "m"]
      | withTyCon = declareTyVar ["m"]
      | otherwise = []
    declareTyVar = map (PlainTV . mkName)
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    derive className = DerivClause Nothing [ConT className]
    cons ConsD {cName, cFields} = RecC (mkName cName) (map declareField cFields)
      where
        declareField FieldD {fieldNameD, fieldTypeD} = (fieldName, defBang, fieldType)
          where
            fieldName = mkName fieldNameD
            fieldType = genFieldT False fieldTypeD
              where
                monadVar = VarT $ mkName "m"
                subscriptionVar = VarT $ mkName "subscriptionM"
                ---------------------------
                genFieldT resM (ListD td) = AppT (ConT ''[]) (genFieldT resM td)
                genFieldT resM (MaybeD td) = AppT (ConT ''Maybe) (genFieldT resM td)
                genFieldT True (BaseD name) = AppT (ConT (mkName name)) monadVar
                genFieldT False (BaseD name)
                  | gqlKind == Just KindUnion = AppT (ConT (mkName name)) monadVar
                genFieldT False (BaseD name) = ConT (mkName name)
                genFieldT _ (ResD arg resKind td) = AppT (AppT arrowType argType) (resultType resKind)
                  where
                    argType = ConT $ mkName arg
                    arrowType = ConT ''FUNC
                    resultType _
                      | isSubscription = AppT subscriptionVar (genFieldT True td)
                    resultType TypeVarResolver = AppT monadVar (genFieldT True td)
                    resultType _ = AppT monadVar (genFieldT False td)
