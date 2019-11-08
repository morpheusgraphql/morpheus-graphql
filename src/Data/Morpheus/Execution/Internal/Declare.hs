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
  , tyConArgs
  ) where

import           Data.Maybe                             (maybe)
import           Data.Text                              (pack, unpack)
import           GHC.Generics                           (Generic)
import           Language.Haskell.TH

-- MORPHEUS
import           Data.Morpheus.Execution.Internal.Utils (nameSpaceType, nameSpaceWith)
import           Data.Morpheus.Types.Internal.Data      (ArgsType (..), DataField (..), DataTypeKind (..),
                                                         DataTypeKind (..), TypeAlias (..), TypeWrapper (..),
                                                         isOutputObject, isSubscription)
import           Data.Morpheus.Types.Internal.DataD     (ConsD (..), TypeD (..))
import           Data.Morpheus.Types.Internal.Resolver  (UnSubResolver)

type Arrow = (->)

declareTypeAlias :: Bool -> TypeAlias -> Type
declareTypeAlias isSub TypeAlias {aliasTyCon, aliasWrappers, aliasArgs} = wrappedT aliasWrappers
  where
    wrappedT :: [TypeWrapper] -> Type
    wrappedT (TypeList:xs)  = AppT (ConT ''[]) $ wrappedT xs
    wrappedT (TypeMaybe:xs) = AppT (ConT ''Maybe) $ wrappedT xs
    wrappedT []          = decType aliasArgs
    ------------------------------------------------------
    typeName = ConT (mkName $ unpack aliasTyCon)
    --------------------------------------------
    decType _
      | isSub = AppT typeName (AppT (ConT ''UnSubResolver) (VarT $ mkName "m"))
    decType (Just par) = AppT typeName (VarT $ mkName $ unpack par)
    decType _ = typeName

tyConArgs :: DataTypeKind -> [String]
tyConArgs kindD
  | isOutputObject kindD || kindD == KindUnion = ["m"]
  | otherwise = []

-- declareType
declareType :: Bool -> Maybe DataTypeKind -> [Name] -> TypeD -> Dec
declareType namespace kindD derivingList TypeD {tName, tCons, tNamespace} =
  DataD [] (genName tName) tVars Nothing (map cons tCons) $ map derive (''Generic : derivingList)
  where
    genName = mkName . nameSpaceType (map pack tNamespace) . pack
    tVars = maybe [] (declareTyVar . tyConArgs) kindD
      where
        declareTyVar = map (PlainTV . mkName)
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    derive className = DerivClause Nothing [ConT className]
    cons ConsD {cName, cFields} = RecC (genName cName) (map declareField cFields)
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
                    arrowType = ConT ''Arrow
                ------------------------------------------------
                fType isResolver
                  | maybe False isSubscription kindD = AppT monadVar result
                  | isResolver = AppT monadVar result
                  | otherwise = result
                ------------------------------------------------
                result = declareTypeAlias (maybe False isSubscription kindD) fieldType
