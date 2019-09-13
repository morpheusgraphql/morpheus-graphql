{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Execution.Document.Introspect
  ( deriveObjectRep
  ) where

import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Server.Introspect (Introspect (..), buildType, updateLib)
import           Data.Morpheus.Types.GQLType               (GQLType (..))
import           Data.Morpheus.Types.Internal.Data         (DataField (..), DataFullType (..), DataTypeWrapper (..),
                                                            Key)
import           Data.Morpheus.Types.Internal.DataD        (AppD (..), ConsD (..), FieldD (..), TypeD (..))
import           Data.Proxy                                (Proxy (..))
import           Data.Text                                 (Text, pack)

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
        body = [|updateLib (typeBuilder cFields) [] (Proxy :: (Proxy $(typeName)))|]
deriveObjectRep _ = pure []

typeBuilder :: GQLType a => [FieldD] -> (Proxy a -> DataFullType)
typeBuilder fields = InputObject . buildType (buildFields fields)

buildFields :: [FieldD] -> [(Key, DataField)]
buildFields = map buildField
  where
    buildField FieldD {fieldNameD, fieldTypeD} =
      ( pack fieldNameD
      , DataField {fieldName = pack fieldNameD, fieldArgs = [], fieldTypeWrappers, fieldType, fieldHidden = False})
      where
        (fieldTypeWrappers, fieldType) = appDToField fieldTypeD

appDToField :: AppD String -> ([DataTypeWrapper], Text)
appDToField = appDToField []
  where
    appDToField wrappers (MaybeD (ListD td))   = appDToField (ListType : wrappers) td
    appDToField wrappers (MaybeD (MaybeD td))  = appDToField wrappers (MaybeD td)
    appDToField wrappers (MaybeD (BaseD name)) = (wrappers, pack name)
    ----------------------------------- NONNULL
    appDToField wrappers (ListD td)            = appDToField ([NonNullType, ListType] <> wrappers) td
    appDToField wrappers (BaseD name)          = (NonNullType : wrappers, pack name)
