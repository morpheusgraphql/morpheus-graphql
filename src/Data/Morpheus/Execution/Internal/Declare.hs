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

import           Data.Text                              (unpack)
import           GHC.Generics                           (Generic)
import           Language.Haskell.TH

-- MORPHEUS
import           Data.Morpheus.Execution.Internal.Utils (nameSpaceWith)
import           Data.Morpheus.Types.Internal.Data      (ArgsType (..), DataField (..), DataTypeKind (..), KindD (..),
                                                         TypeAlias (..), WrapperD (..), unKindD)
import           Data.Morpheus.Types.Internal.DataD     (ConsD (..), TypeD (..))

type FUNC = (->)

declareType :: [Name] -> TypeD -> Dec
declareType = declareGQLT False Nothing

declareTypeAlias :: TypeAlias -> Type
declareTypeAlias TypeAlias {aliasTyCon, aliasWrappers, aliasArgs} = wrappedT aliasWrappers
  where
    wrappedT :: [WrapperD] -> Type
    wrappedT (ListD:xs)  = AppT (ConT ''[]) $ wrappedT xs
    wrappedT (MaybeD:xs) = AppT (ConT ''Maybe) $ wrappedT xs
    wrappedT []          = decType aliasArgs
    ------------------------------------------------------
    typeName = ConT (mkName $ unpack aliasTyCon)
    --------------------------------------------
    decType (Just par) = AppT typeName (VarT $ mkName $ unpack par)
    decType _          = typeName

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
        declareField DataField {fieldName, fieldArgsType, fieldType} = (fName, defBang, fiType)
          where
            fName
              | namespace = mkName (nameSpaceWith tName (unpack fieldName))
              | otherwise = mkName (unpack fieldName)
            fiType = genFieldT fieldArgsType
              where
                monadVar = VarT $ mkName "m"
                subscriptionVar = VarT $ mkName "subscriptionM"
                ---------------------------
                genFieldT Nothing = fType False
                genFieldT (Just ArgsType {argsTypeName}) = AppT (AppT arrowType argType) (fType True)
                  where
                    argType = ConT $ mkName (unpack argsTypeName)
                    arrowType = ConT ''FUNC
                ------------------------------------------------
                fType isResolver
                  | isSubscription = AppT subscriptionVar result
                  | isResolver = AppT monadVar result
                  | otherwise = result
                ------------------------------------------------
                result = declareTypeAlias fieldType
