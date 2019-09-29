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
  , tyConArgs
  ) where

import           Data.Maybe                             (maybe)
import           Data.Text                              (unpack)
import           GHC.Generics                           (Generic)
import           Language.Haskell.TH

-- MORPHEUS
import           Data.Morpheus.Execution.Internal.Utils (nameSpaceWith)
import           Data.Morpheus.Types.Internal.Data      (ArgsType (..), DataField (..), DataTypeKind (..), KindD (..),
                                                         TypeAlias (..), WrapperD (..), isSubscription, unKindD)
import           Data.Morpheus.Types.Internal.DataD     (ConsD (..), TypeD (..))
import           Data.Morpheus.Types.Resolver           (UnSubResolver)

type FUNC = (->)

declareType :: [Name] -> TypeD -> Dec
declareType = declareGQLT False Nothing

declareTypeAlias :: Bool -> TypeAlias -> Type
declareTypeAlias isSub TypeAlias {aliasTyCon, aliasWrappers, aliasArgs} = wrappedT aliasWrappers
  where
    wrappedT :: [WrapperD] -> Type
    wrappedT (ListD:xs)  = AppT (ConT ''[]) $ wrappedT xs
    wrappedT (MaybeD:xs) = AppT (ConT ''Maybe) $ wrappedT xs
    wrappedT []          = decType aliasArgs
    ------------------------------------------------------
    typeName = ConT (mkName $ unpack aliasTyCon)
    --------------------------------------------
    decType _
      | isSub = AppT typeName (AppT (ConT ''UnSubResolver) (VarT $ mkName "m"))
    decType (Just par) = AppT typeName (VarT $ mkName $ unpack par)
    decType _ = typeName

tyConArgs :: KindD -> [String]
tyConArgs kindD
  | isSubscription kindD || gqlKind == KindObject || gqlKind == KindUnion = ["m"]
  | otherwise = []
  where
    gqlKind = unKindD kindD

-- declareType
declareGQLT :: Bool -> Maybe KindD -> [Name] -> TypeD -> Dec
declareGQLT namespace kindD derivingList TypeD {tName, tCons} =
  DataD [] (mkName tName) tVars Nothing (map cons tCons) $ map derive (''Generic : derivingList)
  where
    tVars = maybe [] (declareTyVar . tyConArgs) kindD
      where
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
                ---------------------------
                genFieldT Nothing = fType False
                genFieldT (Just ArgsType {argsTypeName}) = AppT (AppT arrowType argType) (fType True)
                  where
                    argType = ConT $ mkName (unpack argsTypeName)
                    arrowType = ConT ''FUNC
                ------------------------------------------------
                fType isResolver
                  | maybe False isSubscription kindD = AppT monadVar result
                  | isResolver = AppT monadVar result
                  | otherwise = result
                ------------------------------------------------
                result = declareTypeAlias (maybe False isSubscription kindD) fieldType
