{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.TH.Declare.GQLType
  ( deriveGQLType,
  )
where

--
-- MORPHEUS
import Data.Morpheus.Internal.TH
  ( applyVars,
    instanceHeadT,
    instanceProxyFunD,
    toName,
    tyConArgs,
    typeInstanceDec,
  )
import Data.Morpheus.Server.Internal.TH.Types (ServerTypeDefinition (..))
import Data.Morpheus.Server.Internal.TH.Utils
  ( constraintTypeable,
    kindName,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
  )
import Data.Morpheus.Types (Resolver, interface)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    QUERY,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    isObject,
  )
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import Language.Haskell.TH

interfaceF :: Name -> ExpQ
interfaceF name = [|interface (Proxy :: (Proxy ($(conT name) (Resolver QUERY () Maybe))))|]

introspectInterface :: TypeName -> ExpQ
introspectInterface = interfaceF . toName

deriveGQLType :: ServerTypeDefinition cat -> Q [Dec]
deriveGQLType ServerTypeDefinition {tName, tKind, typeOriginal} =
  pure <$> instanceD (cxt constrains) iHead (functions <> typeFamilies)
  where
    functions =
      map
        instanceProxyFunD
        [ ('__typeName, [|tName|]),
          ('description, [|tDescription|]),
          ('implements, implementsFunc)
        ]
      where
        tDescription = typeOriginal >>= typeDescription
        implementsFunc = listE $ map introspectInterface (interfacesFrom typeOriginal)
    --------------------------------
    typeArgs = tyConArgs tKind
    --------------------------------
    iHead = instanceHeadT ''GQLType tName typeArgs
    headSig = applyVars tName typeArgs
    ---------------------------------------------------
    constrains = map constraintTypeable typeArgs
    -------------------------------------------------
    typeFamilies
      | isObject tKind = [deriveKIND, deriveCUSTOM]
      | otherwise = [deriveKIND]
      where
        deriveCUSTOM = deriveInstance ''CUSTOM ''TRUE
        deriveKIND = deriveInstance ''KIND (kindName tKind)
        -------------------------------------------------------
        deriveInstance :: Name -> Name -> Q Dec
        deriveInstance insName tyName = do
          typeN <- headSig
          pure $ typeInstanceDec insName typeN (ConT tyName)

interfacesFrom :: Maybe (TypeDefinition ANY) -> [TypeName]
interfacesFrom (Just TypeDefinition {typeContent = DataObject {objectImplements}}) = objectImplements
interfacesFrom _ = []
