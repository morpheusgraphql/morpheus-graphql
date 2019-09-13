{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Execution.Document.Introspection
  ( deriveObjectRep
  ) where

import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Server.Introspect (Introspect (..))
import           Data.Morpheus.Types.Internal.Data         (DataField (..))
import           Data.Morpheus.Types.Internal.DataD        (ConsD (..), FieldD (..), TypeD (..))
import           Data.Text                                 (pack)

deriveObjectRep :: TypeD -> Q [Dec]
deriveObjectRep TypeD {tName, tCons = [ConsD {cFields}]} = pure <$> instanceD (cxt []) appHead methods
  where
    appHead = appT (appT (appT classT typeT) aT) bT
      where
        classT = conT ''Introspect
        typeT = conT $ mkName tName
        bT = varT (mkName "a")
        aT = varT (mkName "a")
     -- objectFieldTypes :: Proxy rep -> [((Text, DataField t), TypeUpdater)]
    fields = map genField cFields
      where
        genField FieldD {fieldNameD} = [|((fieldNameD, field), pure)|]
          where
            field =
              DataField
                { fieldArgs = []
                , fieldName = pack fieldNameD
                , fieldType = pack "JOE"
                , fieldTypeWrappers = []
                , fieldHidden = False
                }
    methods = [funD 'introspect [clause args (normalB body) []]]
      where
        args = [varP (mkName "_")]
        body = varE 'pure
deriveObjectRep _ = pure []
--
--
--instance InputObjectConstraint a => Introspect a INPUT_OBJECT InputType where
--  __field _ = buildField (Proxy @a) ()
--  introspect _ = updateLib (InputObject . buildType fields') stack' (Proxy @a)
--    where
--      (fields', stack') = unzip $ objectFieldTypes (Proxy @(Rep a))
--
--
--class Introspect a kind args where
--  __field :: Context a kind args -> Text -> DataField args
--    --   generates data field representation of object field
--    --   according to parameter 'args' it could be
--    --   * input object field: if args is '()'
--    --   * object: if args is 'DataArguments'
--
--
--  introspect :: Context a kind args -> TypeUpdater -- Generates internal GraphQL Schema
-- type TypeUpdater = DataTypeLib -> SchemaValidation DataTypeLib
