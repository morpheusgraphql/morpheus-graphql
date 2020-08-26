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
  ( apply,
    applyVars,
    funDProxy,
    toName,
    tyConArgs,
    typeInstanceDec,
  )
import Data.Morpheus.Server.Internal.TH.Types
  ( ServerDecContext (..),
    ServerTypeDefinition (..),
  )
import Data.Morpheus.Server.Internal.TH.Utils
  ( kindName,
    mkTypeableConstraints,
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

deriveGQLType :: ServerDecContext -> ServerTypeDefinition cat s -> Q [Dec]
deriveGQLType
  ServerDecContext {namespace}
  ServerTypeDefinition {tName, tKind, typeOriginal} =
    pure <$> instanceD constrains iHead (functions <> typeFamilies)
    where
      functions =
        funDProxy
          [ ('__typeName, [|tName|]),
            ('description, [|tDescription|]),
            ('implements, implementsFunc),
            ('hasNamespace, hasNamespaceFunc)
          ]
        where
          tDescription = typeOriginal >>= typeDescription
          implementsFunc = listE $ map introspectInterface (interfacesFrom typeOriginal)
          hasNamespaceFunc
            | namespace = [|Just tName|]
            | otherwise = [|Nothing|]
      --------------------------------
      typeArgs = tyConArgs tKind
      --------------------------------
      iHead = apply ''GQLType [applyVars tName typeArgs]
      headSig = applyVars tName typeArgs
      ---------------------------------------------------
      constrains = mkTypeableConstraints typeArgs
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

interfacesFrom :: Maybe (TypeDefinition ANY s) -> [TypeName]
interfacesFrom (Just TypeDefinition {typeContent = DataObject {objectImplements}}) = objectImplements
interfacesFrom _ = []

-- [(FieldDefinition, TypeUpdater)]
-- deriveObjectRep :: ServerTypeDefinition cat s -> Q [Dec]
-- deriveObjectRep
--   ServerTypeDefinition
--     { tName,
--       tCons = [ConsD {cFields}],
--       tKind
--     } =
--     pure <$> instanceD constrains iHead methods
--     where
--       mainTypeName = applyVars tName typeArgs
--       typeArgs = tyConArgs tKind
--       constrains = mkTypeableConstraints typeArgs
--       -----------------------------------------------
--       iHead = apply ''DeriveTypeContent [instCat, conT ''TRUE, mainTypeName]
--       instCat
--         | tKind == KindInputObject =
--           conT ''IN
--         | otherwise = conT ''OUT
--       methods = [funDSimple 'deriveTypeContent [_', _2'] body]
--         where
--           body
--             | tKind == KindInputObject =
--               [|
--                 deriveInputObject
--                   $(buildFields cFields)
--                   $(buildTypes instCat cFields)
--                 |]
--             | otherwise =
--               [|
--                 deriveOutputObject
--                   $(proxy)
--                   $(buildFields cFields)
--                   $(buildTypes instCat cFields)
--                 |]
--           proxy = [|(Proxy :: Proxy $(mainTypeName))|]
-- deriveObjectRep _ = pure []

-- deriveInputObject ::
--   [FieldDefinition IN s] ->
--   [TypeUpdater] ->
--   ( TypeContent TRUE IN s,
--     [TypeUpdater]
--   )
-- deriveInputObject fields typeUpdates =
--   (DataInputObject (unsafeFromFields fields), typeUpdates)

-- deriveOutputObject ::
--   (GQLType a) =>
--   Proxy a ->
--   [FieldDefinition OUT s] ->
--   [TypeUpdater] ->
--   ( TypeContent TRUE OUT s,
--     [TypeUpdater]
--   )
-- deriveOutputObject proxy fields typeUpdates =
--   ( DataObject
--       (interfaceNames proxy)
--       (unsafeFromFields fields),
--     interfaceTypes proxy : typeUpdates
--   )

-- interfaceNames :: GQLType a => Proxy a -> [TypeName]
-- interfaceNames = map fst . implements

-- interfaceTypes :: GQLType a => Proxy a -> TypeUpdater
-- interfaceTypes = concatUpdates . map snd . implements

-- buildTypes :: TypeQ -> [FieldDefinition cat s] -> ExpQ
-- buildTypes cat = listE . concatMap (introspectField cat)

-- introspectField :: TypeQ -> FieldDefinition cat s -> [ExpQ]
-- introspectField cat FieldDefinition {fieldType, fieldContent} =
--   [|introspect $(proxyRepT cat fieldType)|] : inputTypes fieldContent
--   where
--     inputTypes :: Maybe (FieldContent TRUE cat s) -> [ExpQ]
--     inputTypes (Just (FieldArgs ArgumentsDefinition {argumentsTypename = Just argsTypeName}))
--       | argsTypeName /= "()" = [[|deriveCustomInputObjectType (argsTypeName, $(proxyT tAlias))|]]
--       where
--         tAlias = TypeRef {typeConName = argsTypeName, typeWrappers = [], typeArgs = Nothing}
--     inputTypes _ = []

-- proxyRepT :: TypeQ -> TypeRef -> Q Exp
-- proxyRepT cat TypeRef {typeConName, typeArgs} = [|(ProxyRep :: ProxyRep $(cat) $(genSig typeArgs))|]
--   where
--     genSig (Just m) = appT (toCon typeConName) (toVarT m)
--     genSig _ = toCon typeConName

-- proxyT :: TypeRef -> Q Exp
-- proxyT TypeRef {typeConName, typeArgs} = [|(Proxy :: Proxy $(genSig typeArgs))|]
--   where
--     genSig (Just m) = appT (toCon typeConName) (toVarT m)
--     genSig _ = toCon typeConName

-- buildFields :: [FieldDefinition cat s] -> ExpQ
-- buildFields = listE . map buildField
--   where
--     buildField f@FieldDefinition {fieldType} = [|f {fieldType = fieldType {typeConName = __typeName $(proxyT fieldType)}}|]
