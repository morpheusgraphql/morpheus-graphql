{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.Document.GQLType
  ( deriveGQLType,
  )
where

--
-- MORPHEUS
import Data.Morpheus.Internal.TH
  ( instanceHeadT,
    instanceProxyFunD,
    mkTypeName,
    tyConArgs,
    typeInstanceDec,
    typeT,
  )
import Data.Morpheus.Kind
  ( ENUM,
    INPUT,
    INTERFACE,
    OUTPUT,
    SCALAR,
    WRAPPER,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    TRUE,
  )
import Data.Morpheus.Types (Resolver, interface)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    DataTypeKind (..),
    GQLTypeD (..),
    Meta (..),
    QUERY,
    TypeContent (..),
    TypeD (..),
    TypeDefinition (..),
    TypeName,
    isObject,
  )
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import Data.Typeable (Typeable)
import Language.Haskell.TH

interfaceF :: Name -> ExpQ
interfaceF name = [|interface (Proxy :: (Proxy ($(conT name) (Resolver QUERY () Maybe))))|]

introspectInterface :: TypeName -> ExpQ
introspectInterface = interfaceF . mkTypeName

deriveGQLType :: GQLTypeD -> Q [Dec]
deriveGQLType GQLTypeD {typeD = TypeD {tName, tMeta, tKind}, typeOriginal} =
  pure <$> instanceD (cxt constrains) iHead (functions <> typeFamilies)
  where
    functions =
      map
        instanceProxyFunD
        [ ('__typeName, [|tName|]),
          ('description, descriptionValue),
          ('implements, implementsFunc)
        ]
      where
        implementsFunc = listE $ map introspectInterface (interfacesFrom (Just typeOriginal))
        descriptionValue = case tMeta >>= metaDescription of
          Nothing -> [|Nothing|]
          Just desc -> [|Just desc|]
    --------------------------------
    typeArgs = tyConArgs tKind
    --------------------------------
    iHead = instanceHeadT ''GQLType tName typeArgs
    headSig = typeT (mkTypeName tName) typeArgs
    ---------------------------------------------------
    constrains = map conTypeable typeArgs
      where
        conTypeable name = typeT ''Typeable [name]
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

kindName :: DataTypeKind -> Name
kindName KindObject {} = ''OUTPUT
kindName KindScalar = ''SCALAR
kindName KindEnum = ''ENUM
kindName KindUnion = ''OUTPUT
kindName KindInputObject = ''INPUT
kindName KindList = ''WRAPPER
kindName KindNonNull = ''WRAPPER
kindName KindInputUnion = ''INPUT
kindName KindInterface = ''INTERFACE

interfacesFrom :: Maybe (TypeDefinition ANY) -> [TypeName]
interfacesFrom (Just TypeDefinition {typeContent = DataObject {objectImplements}}) = objectImplements
interfacesFrom _ = []
