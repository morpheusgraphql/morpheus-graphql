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
  , declareGQLT
  ) where

import           GHC.Generics                           (Generic)
import           Language.Haskell.TH

-- MORPHEUS
import           Data.Morpheus.Execution.Internal.Utils (nameSpaceWith)
import           Data.Morpheus.Types.Internal.Data      (DataTypeKind (..))
import           Data.Morpheus.Types.Internal.DataD     (AppD (..), ConsD (..), FieldD (..), KindD (..), TypeD (..),
                                                         unKindD)

type FUNC = (->)

declareType :: [Name] -> TypeD -> Dec
declareType = declareGQLT False Nothing

wrappedT :: AppD (String, [String]) -> Type
wrappedT (ListD td)            = AppT (ConT ''[]) (wrappedT td)
wrappedT (MaybeD td)           = AppT (ConT ''Maybe) (wrappedT td)
wrappedT (BaseD (name, [par])) = AppT (ConT (mkName name)) (VarT $ mkName par)
wrappedT (BaseD (name, _))     = ConT (mkName name)

-- declareType
declareGQLT :: Bool -> Maybe KindD -> [Name] -> TypeD -> Dec
declareGQLT namespace kindD derivingList TypeD {tName, tCons} =
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
        declareField FieldD {fieldNameD, fieldArgsD, fieldTypeD} = (fieldName, defBang, fieldType)
          where
            fieldName
              | namespace = mkName (nameSpaceWith tName fieldNameD)
              | otherwise = mkName fieldNameD
            fieldType = genFieldT fieldArgsD
              where
                monadVar = VarT $ mkName "m"
                subscriptionVar = VarT $ mkName "subscriptionM"
                ---------------------------
                genFieldT Nothing = fType False
                genFieldT (Just (argsTypeName, _)) = AppT (AppT arrowType argType) (fType True)
                  where
                    argType = ConT $ mkName argsTypeName
                    arrowType = ConT ''FUNC
                ------------------------------------------------
                fType isResolver
                  | isSubscription = AppT subscriptionVar result
                  | isResolver = AppT monadVar result
                  | otherwise = result
                ------------------------------------------------
                result = wrappedT fieldTypeD
