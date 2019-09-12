{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Execution.Document.Introspection
  ( deriveObjectRep
  ) where

import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Server.Introspect (ObjectRep (..))
import           Data.Morpheus.Types.Internal.Data         (DataField (..))
import           Data.Morpheus.Types.Internal.DataD        (ConsD (..), FieldD (..), GQLTypeD (..), TypeD (..))
import           Data.Text                                 (pack)

deriveObjectRep :: TypeD -> Q [Dec]
deriveObjectRep TypeD {tName, tCons = [ConsD {cFields}]} =
  pure <$> instanceD (cxt []) appHead methods
  where
    appHead = appT (appT (conT ''ObjectRep) (conT $ mkName tName)) (conT ''())
     -- objectFieldTypes :: Proxy rep -> [((Text, DataField t), TypeUpdater)]
    genField FieldD {fieldNameD} = [|((fieldNameD, field), pure)|]
      where
        field =
          DataField
            { fieldArgs = ()
            , fieldName = pack fieldNameD
            , fieldType = pack "JOE"
            , fieldTypeWrappers = []
            , fieldHidden = False
            }
    fields = map genField cFields
    methods = [funD 'objectFieldTypes [clause [varP (mkName "_")] (normalB (listE fields)) []]]
deriveObjectRep _ = pure []
