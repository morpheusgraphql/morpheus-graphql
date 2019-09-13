{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Execution.Document.Introspection
  ( deriveObjectRep
  ) where

import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Server.Introspect (Introspect (..), buildType, updateLib)
import           Data.Morpheus.Types.GQLType               (GQLType (..))
import           Data.Morpheus.Types.Internal.Data         (DataField (..), DataFullType (..), DataTypeWrapper (..),
                                                            Key)
import           Data.Morpheus.Types.Internal.DataD        (ConsD (..), FieldD (..), TypeD (..))
import           Data.Proxy                                (Proxy (..))
import           Data.Text                                 (pack)

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
      , DataField
          { fieldName = pack fieldNameD
          , fieldArgs = []
          , fieldTypeWrappers = [NonNullType]
          , fieldType = pack "String"
          , fieldHidden = False
          })
