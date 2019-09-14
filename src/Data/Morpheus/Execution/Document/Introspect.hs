{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Execution.Document.Introspect
  ( deriveArguments
  , deriveIntrospect
  ) where

import           Data.Proxy                                (Proxy (..))
import           Data.Semigroup                            ((<>))
import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Server.Introspect (Introspect (..), ObjectFields (..), buildType, updateLib)
import           Data.Morpheus.Types.GQLType               (GQLType (__typeName))
import           Data.Morpheus.Types.Internal.Data         (DataField (..), DataFullType (..), DataTypeWrapper (..))
import           Data.Morpheus.Types.Internal.DataD        (AppD (..), ConsD (..), FieldD (..), TypeD (..))

-- [((Text, DataField), TypeUpdater)]
deriveArguments :: TypeD -> Q [Dec]
deriveArguments TypeD {tName, tCons = [ConsD {cFields}]} = pure <$> instanceD (cxt []) appHead methods
  where
    appHead = appT classT typeT
      where
        classT = conT ''ObjectFields
        typeT = conT $ mkName tName
    methods = [funD 'objectFields [clause argsE (normalB body) []]]
      where
        argsE = [varP (mkName "_")]
        body = [|($(fields), $(types))|]
        types = buildTypes cFields
        fields = buildFields cFields
deriveArguments _ = pure []

deriveIntrospect :: TypeD -> Q [Dec]
deriveIntrospect TypeD {tName, tCons = [ConsD {cFields}]} = pure <$> instanceD (cxt []) appHead methods
  where
    typeName = conT $ mkName tName
    appHead = appT classT typeT
      where
        classT = conT ''Introspect
        typeT = conT $ mkName tName
    methods = [funD 'introspect [clause argsE (normalB body) []]]
      where
        argsE = [varP (mkName "_")]
        body = [|updateLib $(typeBuilder) $(types) (Proxy :: (Proxy $(typeName)))|]
        types = buildTypes cFields
        typeBuilder = [|InputObject . buildType $(buildFields cFields)|]
deriveIntrospect _ = pure []

buildTypes :: [FieldD] -> ExpQ
buildTypes = listE . map introspectType
  where
    introspectType fieldD = [|introspect (Proxy :: Proxy $(lookupType fieldD))|]
      where
        lookupType FieldD {fieldTypeD} = conT $ mkName $ snd $ appDToField fieldTypeD

buildFields :: [FieldD] -> ExpQ
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

appDToField :: AppD String -> ([DataTypeWrapper], String)
appDToField = appDToField []
  where
    appDToField wrappers (MaybeD (ListD td))   = appDToField (wrappers <> [ListType]) td
    appDToField wrappers (ListD td)            = appDToField (wrappers <> [NonNullType, ListType]) td
    appDToField wrappers (MaybeD (MaybeD td))  = appDToField wrappers (MaybeD td)
    appDToField wrappers (MaybeD (BaseD name)) = (wrappers, name)
    appDToField wrappers (BaseD name)          = (wrappers <> [NonNullType], name)
