{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE GADTs                 #-}

module Data.Morpheus.Execution.Internal.Declare
  ( declareType
  , isEnum
  , tyConArgs
  , Scope(..)
  )
where

import           Data.Maybe                     ( maybe )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( unpack )
import           GHC.Generics                   ( Generic )
import           Language.Haskell.TH

-- MORPHEUS
import           Data.Morpheus.Execution.Internal.Utils
                                                ( nameSpaceType
                                                , nameSpaceWith
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( FieldDefinition(..)
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
                                                , ArgumentsDefinition(..)
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

data Scope = CLIENT | SERVER
  deriving Eq

-- declareType
declareType :: Scope -> Bool -> Maybe DataTypeKind -> [Name] -> TypeD -> Dec
declareType scope namespace kindD derivingList TypeD { tName, tCons, tNamespace } =
  DataD [] (genName tName) tVars Nothing cons
    $ map derive (''Generic : derivingList)
 where
  genName = mkName . unpack . nameSpaceType tNamespace
  tVars   = maybe [] (declareTyVar . tyConArgs) kindD
    where declareTyVar = map (PlainTV . mkName . unpack)
  defBang = Bang NoSourceUnpackedness NoSourceStrictness
  derive className = DerivClause Nothing [ConT className]
  prefixEnum = isEnum tCons && (scope == CLIENT || namespace)
  cons
    | prefixEnum = map consE tCons
    | otherwise  = map consR tCons
  consE ConsD { cName }          = NormalC (genName $ tName <> cName) []
  consR ConsD { cName, cFields } = RecC (genName cName)
                                        (map declareField cFields)

   where
    declareField FieldDefinition { fieldName, fieldArgs, fieldType } =
      (fName, defBang, fiType)
     where
      fName | namespace = mkName $ unpack (nameSpaceWith tName fieldName)
            | otherwise = mkName (unpack fieldName)
      fiType = genFieldT fieldArgs
       where
        monadVar = VarT $ mkName $ unpack m_
        ---------------------------
        genFieldT ArgumentsDefinition {  argumentsTypename = Just argsTypename } = AppT
          (AppT arrowType argType)
          (AppT monadVar result)
         where
          argType   = ConT $ mkName (unpack argsTypename)
          arrowType = ConT ''Arrow
        genFieldT _
          | (isOutputObject <$> kindD) == Just True = AppT monadVar result
          | otherwise                             = result
        ------------------------------------------------
        result = declareTypeRef (maybe False isSubscription kindD) fieldType

isEnum :: [ConsD] -> Bool
isEnum = all (null . cFields)