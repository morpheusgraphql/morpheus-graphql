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
          where
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
deriveObjectRep _ = pure []
--
--
--instance InputObjectConstraint a => Introspect a INPUT_OBJECT InputType where
--  __field _ = buildField (Proxy @a) ()
--  introspect _ = updateLib (InputObject . buildType fields') stack' (Proxy @a)
--  introspect :: Context a kind args -> TypeUpdater -- Generates internal GraphQL Schema
-- type TypeUpdater = DataTypeLib -> SchemaValidation DataTypeLib
