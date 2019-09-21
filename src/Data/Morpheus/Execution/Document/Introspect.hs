{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Execution.Document.Introspect
  ( deriveObjectRep
  ) where

import           Data.Proxy                                (Proxy (..))
import           Data.Typeable                             (Typeable)
import           Language.Haskell.TH

-- MORPHEUS
import           Data.Morpheus.Execution.Document.GQLType  (genTypeArgs)
import           Data.Morpheus.Execution.Server.Introspect (Introspect (..), ObjectFields (..))
import           Data.Morpheus.Types.GQLType               (GQLType (__typeName))
import           Data.Morpheus.Types.Internal.Data         (DataField (..), KindD, ResolverKind, hsToGQLWrapper)
import           Data.Morpheus.Types.Internal.DataD        (ConsD (..), FieldD (..), TypeD (..))
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
        body = [|($(buildFields cFields), $(buildTypes cFields))|]
deriveObjectRep _ = pure []

buildTypes :: [FieldD] -> ExpQ
buildTypes = listE . concatMap introspectField
  where
    introspectField FieldD {fieldTypeD} = [[|introspect $(proxyT $ snd (hsToGQLWrapper fieldTypeD))|]]

proxyT :: (String, [String]) -> Q Exp
proxyT t = [|(Proxy :: Proxy $(genSig t))|]
  where
    genSig (name, [m]) = appT (conT $ mkName name) (varT $ mkName m)
    genSig (name, _)   = conT $ mkName name

fieldArgsRep :: Maybe (String, ResolverKind) -> Q Exp
fieldArgsRep (Just (name, _)) = [|objectFields $(proxyT (name, []))|]
fieldArgsRep _                = [|([], [])|]

buildFields :: [FieldD] -> ExpQ
buildFields = listE . map buildField
  where
    buildField FieldD {fieldNameD, fieldTypeD, fieldArgsD} =
      [|( fieldNameD
        , DataField
            { fieldName = fieldNameD
            , fieldArgs = fst $(fieldArgsRep fieldArgsD)
            , fieldTypeWrappers
            , fieldType = __typeName $(proxyT fieldType)
            , fieldHidden = False
            })|]
      where
        (fieldTypeWrappers, fieldType) = hsToGQLWrapper fieldTypeD
