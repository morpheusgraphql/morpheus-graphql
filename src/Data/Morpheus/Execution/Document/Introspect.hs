{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Execution.Document.Introspect
  ( deriveObjectRep
  ) where

import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Server.Introspect (Introspect (..), buildType, updateLib)
import           Data.Morpheus.Types.GQLType               (GQLType (__typeName))
import           Data.Morpheus.Types.Internal.Data         (DataField (..), DataFullType (..), DataTypeWrapper (..))
import           Data.Morpheus.Types.Internal.DataD        (AppD (..), ConsD (..), FieldD (..), TypeD (..))
import           Data.Proxy                                (Proxy (..))

deriveObjectRep :: TypeD -> Q [Dec]
deriveObjectRep TypeD {tName, tCons = [ConsD {cFields}]} = pure <$> instanceD (cxt []) appHead methods
  where
    typeName = conT $ mkName tName
    appHead = appT classT typeT
      where
        classT = conT ''Introspect
        typeT = conT $ mkName tName
    methods = [funD 'introspect [clause argsE (normalB body) []]]
      where
        argsE = [varP (mkName "_")]
        body = [| updateLib $(typeBuilder) $(types) (Proxy :: (Proxy $(typeName)))|]
        types = listE $ map introspectType cFields
          where
            introspectType fieldD = [|introspect (Proxy :: Proxy $(lookupType fieldD))|]
              where
                lookupType FieldD {fieldTypeD} = conT $ mkName $ snd $ appDToField fieldTypeD
        typeBuilder = [|InputObject . buildType $(buildFields cFields)|]
          where
            buildFields = listE . map buildField
              where
                buildField FieldD {fieldNameD, fieldTypeD} =
                  [|( fieldNameD
                    , DataField
                        { fieldName = fieldNameD
                        , fieldArgs = []
                        , fieldTypeWrappers
                        , fieldType = __typeName (Proxy :: (Proxy $(conT $ mkName fieldType)))
                        , fieldHidden = False
                        })|]
                  where
                    (fieldTypeWrappers, fieldType) = appDToField fieldTypeD
deriveObjectRep _ = pure []

appDToField :: AppD String -> ([DataTypeWrapper], String)
appDToField = appDToField []
  where
    appDToField wrappers (MaybeD (ListD td))   = appDToField (ListType : wrappers) td
    appDToField wrappers (MaybeD (MaybeD td))  = appDToField wrappers (MaybeD td)
    appDToField wrappers (MaybeD (BaseD name)) = (wrappers, name)
    ----------------------------------- NONNULL
    appDToField wrappers (ListD td)            = appDToField ([NonNullType, ListType] <> wrappers) td
    appDToField wrappers (BaseD name)          = (NonNullType : wrappers, name)
