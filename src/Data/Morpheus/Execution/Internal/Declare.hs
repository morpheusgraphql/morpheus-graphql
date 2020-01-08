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
  )
where

import           Data.Maybe                     ( maybe )
import           Data.Text                      ( unpack )
import           GHC.Generics                   ( Generic )
import           Language.Haskell.TH

-- MORPHEUS
import           Data.Morpheus.Execution.Internal.Utils
                                                ( nameSpaceType
                                                , nameSpaceWith
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( DataField(..)
                                                , DataTypeKind(..)
                                                , DataTypeKind(..)
                                                , TypeRef(..)
                                                , TypeWrapper(..)
                                                , isOutputObject
                                                , isSubscription
                                                , ConsD(..)
                                                , TypeD(..)
                                                , Key
                                                , isOutputObject
                                                , DataArguments(..)
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( UnSubResolver )

type Arrow = (->)


m_ :: Key
m_ = "m"

declareTypeRef :: Bool -> TypeRef -> Type
declareTypeRef isSub TypeRef { typeConName, typeWrappers, typeArgs } = wrappedT
  typeWrappers
 where
  wrappedT :: [TypeWrapper] -> Type
  wrappedT (TypeList  : xs) = AppT (ConT ''[]) $ wrappedT xs
  wrappedT (TypeMaybe : xs) = AppT (ConT ''Maybe) $ wrappedT xs
  wrappedT []               = decType typeArgs
  ------------------------------------------------------
  typeName = ConT (mkName $ unpack typeConName)
  --------------------------------------------
  decType _ | isSub =
    AppT typeName (AppT (ConT ''UnSubResolver) (VarT $ mkName $ unpack m_))
  decType (Just par) = AppT typeName (VarT $ mkName $ unpack par)
  decType _          = typeName

tyConArgs :: DataTypeKind -> [Key]
tyConArgs kindD | isOutputObject kindD || kindD == KindUnion = [m_]
                | otherwise = []

-- declareType
declareType :: Bool -> Maybe DataTypeKind -> [Name] -> TypeD -> Dec
declareType namespace kindD derivingList TypeD { tName, tCons, tNamespace } =
  DataD [] (genName tName) tVars Nothing (map cons tCons)
    $ map derive (''Generic : derivingList)
 where
  genName = mkName . unpack . nameSpaceType tNamespace
  tVars   = maybe [] (declareTyVar . tyConArgs) kindD
    where declareTyVar = map (PlainTV . mkName . unpack)
  defBang = Bang NoSourceUnpackedness NoSourceStrictness
  derive className = DerivClause Nothing [ConT className]
  cons ConsD { cName, cFields } = RecC (genName cName)
                                       (map declareField cFields)
   where
    declareField DataField { fieldName, fieldArgsType, fieldType } =
      (fName, defBang, fiType)
     where
      fName | namespace = mkName $ unpack (nameSpaceWith tName fieldName)
            | otherwise = mkName (unpack fieldName)
      fiType = genFieldT fieldArgsType
       where
        monadVar = VarT $ mkName $ unpack m_
        ---------------------------
        genFieldT NoArguments
          | (isOutputObject <$> kindD) == Just True = AppT monadVar result
          | otherwise                             = result
        genFieldT DataArguments { argumentsTypename } = AppT
          (AppT arrowType argType)
          (AppT monadVar result)
         where
          argType   = ConT $ mkName (unpack argsTypeName)
          arrowType = ConT ''Arrow
        ------------------------------------------------
        result = declareTypeRef (maybe False isSubscription kindD) fieldType
