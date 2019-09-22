{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Execution.Document.Introspect
  ( deriveObjectRep
  ) where

import           Data.Proxy                                (Proxy (..))
import           Data.Text                                 (unpack)
import           Data.Typeable                             (Typeable)
import           Language.Haskell.TH

-- MORPHEUS
import           Data.Morpheus.Execution.Document.GQLType  (genTypeArgs)
import           Data.Morpheus.Execution.Server.Introspect (Introspect (..), ObjectFields (..))
import           Data.Morpheus.Types.GQLType               (GQLType (__typeName))
import           Data.Morpheus.Types.Internal.Data         (ArgsType (..), DataField (..), KindD, TypeAlias (..))
import           Data.Morpheus.Types.Internal.DataD        (ConsD (..), TypeD (..))
import           Data.Morpheus.Types.Internal.TH           (instanceFunD, instanceHeadT, typeT)

-- [((Text, DataField), TypeUpdater)]
deriveObjectRep :: (TypeD, Maybe KindD) -> Q [Dec]
deriveObjectRep (TypeD {tName, tCons = [ConsD {cFields}]}, tKind) =
  pure <$> instanceWithOverlapD overlapping (cxt constrains) iHead methods
  where
    overlapping = Just Overlapping
    typeArgs =
      case tKind of
        Just typeKind -> genTypeArgs typeKind
        Nothing       -> []
    constrains = map conTypeable typeArgs
      where
        conTypeable name = typeT ''Typeable [name]
    -----------------------------------------------
    iHead = instanceHeadT ''ObjectFields tName typeArgs
    methods = [instanceFunD 'objectFields ["_"] body]
      where
        body = [|($(buildFields cFields), concat $(buildTypes cFields))|]
deriveObjectRep _ = pure []

buildTypes :: [DataField] -> ExpQ
buildTypes = listE . concatMap introspectField
  where
    introspectField DataField {fieldType, fieldArgsType} = [| [introspect $(proxyT fieldType)] |] : inputTypes fieldArgsType
      where
        inputTypes (Just ArgsType {argsTypeName})
          | argsTypeName /= "()" = [[| snd $ objectFields $(proxyT tAlias)|]]
          where
            tAlias = TypeAlias {aliasTyCon = argsTypeName, aliasWrappers = [], aliasArgs = Nothing}
        inputTypes _ = []

proxyT :: TypeAlias -> Q Exp
proxyT TypeAlias {aliasTyCon, aliasArgs} = [|(Proxy :: Proxy $(genSig aliasArgs))|]
  where
    genSig (Just m) = appT (conT $ mkName $ unpack aliasTyCon) (varT $ mkName $ unpack m)
    genSig _        = conT $ mkName $ unpack aliasTyCon

buildFields :: [DataField] -> ExpQ
buildFields = listE . map buildField
  where
    buildField DataField {fieldName, fieldArgs, fieldType = alias@TypeAlias {aliasArgs, aliasWrappers}} =
      [|( fName
        , DataField
            { fieldName = fName
            , fieldArgs = fArgs
            , fieldArgsType = Nothing
            , fieldType = TypeAlias {aliasTyCon = __typeName $(proxyT alias), aliasArgs = aArgs, aliasWrappers}
            , fieldHidden = False
            })|]
      where
        fName = unpack fieldName
        fArgs = map (\(k, v) -> (unpack k, v)) fieldArgs
        aArgs = unpack <$> aliasArgs
