{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Execution.Document.Introspection
  ( deriveObjectRep
  ) where

import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Server.Introspect (Introspect (..), updateLib)
import           Data.Morpheus.Types.GQLType               (GQLType (..))
import           Data.Morpheus.Types.Internal.Data         (DataField (..), DataFullType (..), DataType (..))
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
     -- objectFieldTypes :: Proxy rep -> [((Text, DataField t), TypeUpdater)]
    methods = [funD 'introspect [clause argsE (normalB body) []]]
      where
        argsE = [varP (mkName "_")]
        body = [|updateLib $(datatypeE) $(types) $(proxyE)|]
        types = [|[]|]
        fieldsE = listE (map genField cFields)
          where
            genField FieldD {fieldNameD} =
              [|( fieldNameD
                , DataField
                    { fieldArgs = []
                    , fieldName = fieldNameD
                    , fieldType = "JOE"
                    , fieldTypeWrappers = []
                    , fieldHidden = False
                    })|]
        proxyE = [|(Proxy :: (Proxy $(typeName)))|]
        datatypeE =
          [|const $
            InputObject $
            DataType
              { typeName = tName
              , typeFingerprint = __typeFingerprint $(proxyE)
              , typeDescription = tName
              , typeVisibility = True
              , typeData = []
              }|]
       -- updateLib :: GQLType a => (Proxy a -> DataFullType) -> [TypeUpdater] -> Proxy a -> TypeUpdater
--instance ObjectConstraint a => Introspect1 a INPUT_OBJECT where
--  __introspect _ = updateLib  stack' (Proxy @a)
--    where
--      (fields', stack') = unzip $ objectFieldTypes (Proxy @(Rep a))
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
