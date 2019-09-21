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
import           Data.Morpheus.Types.Internal.Data         (DataField (..), KindD, ResolverKind, TypeAlias (..))
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
    introspectField FieldD {fieldTypeD = TypeAlias {aliasTyCon, aliasArgs}} =
      [[|introspect $(proxyT (unpack aliasTyCon) (unpack <$> aliasArgs))|]]

proxyT :: String -> Maybe String -> Q Exp
proxyT name args = [|(Proxy :: Proxy $(genSig args))|]
  where
    genSig (Just m) = appT (conT $ mkName name) (varT $ mkName m)
    genSig _        = conT $ mkName name

fieldArgsRep :: Maybe (String, ResolverKind) -> Q Exp
fieldArgsRep (Just (name, _)) = [|objectFields $(proxyT name Nothing)|]
fieldArgsRep _                = [|([], [])|]

buildFields :: [FieldD] -> ExpQ
buildFields = listE . map buildField
  where
    buildField FieldD {fieldNameD, fieldArgsD, fieldTypeD = TypeAlias {aliasTyCon, aliasArgs, aliasWrappers}} =
      [|( fieldNameD
        , DataField
            { fieldName = fieldNameD
            , fieldArgs = fst $(fieldArgsRep fieldArgsD)
            , fieldTypeWrappers = aliasWrappers
            , fieldType = __typeName $(proxyT (unpack aliasTyCon) (unpack <$> aliasArgs))
            , fieldHidden = False
            })|]
