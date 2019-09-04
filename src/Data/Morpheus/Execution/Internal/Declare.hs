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
import           Data.Morpheus.Types.Internal.DataD (AppD (..), ConsD (..), FieldD (..), TypeD (..))
import           GHC.Generics                       (Generic)

type FUNC = (->)

declareType :: [Name] -> TypeD -> Dec
declareType = __declareType False

declareResolverType :: DataTypeKind -> [Name] -> TypeD -> Dec
declareResolverType KindObject = __declareType True
declareResolverType _          = __declareType False

--
--
__declareType :: Bool -> [Name] -> TypeD -> Dec
__declareType isResolver derivingList TypeD {tName, tCons} =
  DataD [] (mkName tName) tVars Nothing (map cons tCons) $ map derive (''Generic : derivingList)
  where
    tVars
      | isResolver = [PlainTV $ mkName "m"]
      | otherwise = []
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    derive className = DerivClause Nothing [ConT className]
    cons ConsD {cName, cFields} = RecC (mkName cName) (map genField cFields)
      where
        genField FieldD {fieldNameD, fieldTypeD} = (mkName fieldNameD, defBang, genFieldT fieldTypeD)
          where
            genFieldT (ListD td) = AppT (ConT ''[]) (genFieldT td)
            genFieldT (MaybeD td) = AppT (ConT ''Maybe) (genFieldT td)
            genFieldT (BaseD name) = ConT (mkName name)
            genFieldT (ResD arg _ td) = AppT (AppT arrowType argType) resultType
              where
                argType = ConT $ mkName arg
                arrowType = ConT ''FUNC
                resultType = AppT monadVar (AppT (genFieldT td) monadVar)
                monadVar = VarT $ mkName "m"
