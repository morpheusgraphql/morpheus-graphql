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
import           Data.Morpheus.Types.Internal.DataD (AppD (..), ConsD (..), FieldD (..), ResolverKind (..), TypeD (..))
import           GHC.Generics                       (Generic)

type FUNC = (->)

declareType :: [Name] -> TypeD -> Dec
declareType = __declareType Nothing

declareResolverType :: DataTypeKind -> [Name] -> TypeD -> Dec
declareResolverType x = __declareType (Just x)

--
--
__declareType :: Maybe DataTypeKind -> [Name] -> TypeD -> Dec
__declareType gqlKind derivingList TypeD {tName, tCons} =
  DataD [] (mkName tName) tVars Nothing (map cons tCons) $ map derive (''Generic : derivingList)
  where
    withTyCon = gqlKind == Just KindObject || gqlKind == Just KindUnion
    tVars
      | withTyCon = [PlainTV $ mkName "m"]
      | otherwise = []
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    derive className = DerivClause Nothing [ConT className]
    cons ConsD {cName, cFields} = RecC (mkName cName) (map genField cFields)
      where
        genField FieldD {fieldNameD, fieldTypeD} = (mkName fieldNameD, defBang, genFieldT False fieldTypeD)
          where
            monadVar = VarT $ mkName "m"
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
                resultType TypeVarResolver = AppT monadVar (genFieldT True td)
                resultType _               = AppT monadVar (genFieldT False td)
