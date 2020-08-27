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
import Data.Map (fromList)
import Data.Maybe (maybe)
import Data.Morpheus.Internal.TH
  ( apply,
    applyVars,
    funDProxy,
    toName,
    tyConArgs,
    typeInstanceDec,
  )
import Data.Morpheus.Internal.Utils (elems)
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
    ArgumentsDefinition,
    Description,
    Directives,
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    QUERY,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    Value,
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
            ('hasNamespace, hasNamespaceFunc),
            ('fieldValues, fieldValuesFunc)
          ]
        where
          tDescription = typeOriginal >>= typeDescription
          implementsFunc = listE $ map introspectInterface (interfacesFrom typeOriginal)
          hasNamespaceFunc
            | namespace = [|Just tName|]
            | otherwise = [|Nothing|]
          fieldValuesFunc = [|value|]
            where
              value = fromList (maybe [] getTypeMeta typeOriginal)
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

type Meta s =
  ( Maybe Description,
    Directives s,
    Maybe (Value s),
    Maybe (ArgumentsDefinition s)
  )

getTypeMeta :: TypeDefinition c s -> [(FieldName, Meta s)]
getTypeMeta TypeDefinition {typeContent = DataObject {objectFields}} = getFieldMeta <$> elems objectFields
getTypeMeta TypeDefinition {typeContent = DataInputObject {inputObjectFields}} = getFieldMeta <$> elems inputObjectFields
getTypeMeta TypeDefinition {typeContent = DataInterface {interfaceFields}} = getFieldMeta <$> elems interfaceFields
getTypeMeta _ = []

getFieldMeta :: FieldDefinition c s -> (FieldName, Meta s)
getFieldMeta
  FieldDefinition
    { fieldDescription,
      fieldDirectives,
      fieldContent,
      fieldName
    } =
    ( fieldName,
      ( fieldDescription,
        fieldDirectives,
        fieldContent >>= getDefaultValue,
        fieldContent >>= getArgsDef
      )
    )

getDefaultValue :: FieldContent a c s -> Maybe (Value s)
getDefaultValue DefaultInputValue {defaultInputValue} = Just defaultInputValue
getDefaultValue _ = Nothing

getArgsDef :: FieldContent a c s -> Maybe (ArgumentsDefinition s)
getArgsDef (FieldArgs args) = Just args
getArgsDef _ = Nothing
