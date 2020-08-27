{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Declare.GQLType
  ( deriveGQLType,
  )
where

--
-- MORPHEUS
import Control.Applicative (Applicative (..))
import Control.Monad (Monad ((>>=)))
import Data.Functor ((<$>), fmap)
import Data.Map (Map, fromList)
import Data.Maybe (Maybe (..), maybe)
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
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    IN,
    OUT,
    QUERY,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    Value,
  )
import Data.Proxy (Proxy (..))
import Language.Haskell.TH
import Prelude
  ( ($),
    (.),
    otherwise,
  )

interfaceF :: Name -> ExpQ
interfaceF name = [|interface (Proxy :: (Proxy ($(conT name) (Resolver QUERY () Maybe))))|]

introspectInterface :: TypeName -> ExpQ
introspectInterface = interfaceF . toName

deriveGQLType :: ServerDecContext -> ServerTypeDefinition cat s -> Q [Dec]
deriveGQLType
  ServerDecContext {namespace}
  ServerTypeDefinition {tName, tKind, typeOriginal} =
    pure <$> instanceD constrains iHead (typeFamilies : functions)
    where
      functions =
        funDProxy
          [ ('__typeName, [|tName|]),
            ('description, [|tDescription|]),
            ('implements, implementsFunc),
            ('hasNamespace, hasNamespaceFunc),
            ('getDescriptions, fieldDescriptionsFunc),
            ('getFieldDirectives, fieldDirectivesFunc),
            ('getFieldContents, getFieldContentsFunc)
          ]
        where
          tDescription = typeOriginal >>= typeDescription
          implementsFunc = listE $ fmap introspectInterface (interfacesFrom typeOriginal)
          hasNamespaceFunc
            | namespace = [|Just tName|]
            | otherwise = [|Nothing|]
          fieldDescriptionsFunc = [|value|]
            where
              value = fmapFieldValues fieldDescription fieldDescription typeOriginal
          fieldDirectivesFunc = [|value|]
            where
              value = fmapFieldValues (Just . fieldDirectives) (Just . fieldDirectives) typeOriginal
          getFieldContentsFunc = [|value|]
            where
              value =
                fmapFieldValues
                  (fmap getDefaultValue . fieldContent)
                  (fmap getDefaultValue . fieldContent)
                  typeOriginal
      --------------------------------
      typeArgs = tyConArgs tKind
      --------------------------------
      iHead = apply ''GQLType [applyVars tName typeArgs]
      headSig = applyVars tName typeArgs
      ---------------------------------------------------
      constrains = mkTypeableConstraints typeArgs
      -------------------------------------------------
      typeFamilies = deriveInstance ''KIND (kindName tKind)
        where
          deriveInstance :: Name -> Name -> Q Dec
          deriveInstance insName tyName = do
            typeN <- headSig
            pure $ typeInstanceDec insName typeN (ConT tyName)

interfacesFrom :: Maybe (TypeDefinition ANY s) -> [TypeName]
interfacesFrom (Just TypeDefinition {typeContent = DataObject {objectImplements}}) = objectImplements
interfacesFrom _ = []

fmapFieldValues :: (FieldDefinition IN s -> Maybe a) -> (FieldDefinition OUT s -> Maybe a) -> Maybe (TypeDefinition c s) -> Map FieldName a
fmapFieldValues f g = maybe (fromList []) (collectFieldValues f g)

collectFieldValues :: (FieldDefinition IN s -> Maybe a) -> (FieldDefinition OUT s -> Maybe a) -> TypeDefinition c s -> Map FieldName a
collectFieldValues _ g TypeDefinition {typeContent = DataObject {objectFields}} = getFieldValues g objectFields
collectFieldValues f _ TypeDefinition {typeContent = DataInputObject {inputObjectFields}} = getFieldValues f inputObjectFields
collectFieldValues _ g TypeDefinition {typeContent = DataInterface {interfaceFields}} = getFieldValues g interfaceFields
collectFieldValues _ _ _ = fromList []

getFieldValues :: (FieldDefinition c s -> Maybe a) -> FieldsDefinition c s -> Map FieldName a
getFieldValues f = fromList . notNulls . fmap (getFieldValue f) . elems

notNulls :: [(FieldName, Maybe a)] -> [(FieldName, a)]
notNulls [] = []
notNulls ((_, Nothing) : xs) = notNulls xs
notNulls ((name, Just x) : xs) = (name, x) : notNulls xs

getFieldValue :: (FieldDefinition c s -> Maybe a) -> FieldDefinition c s -> (FieldName, Maybe a)
getFieldValue f field = (fieldName field, f field)

getDefaultValue :: FieldContent TRUE c s -> (Maybe (Value s), Maybe (ArgumentsDefinition s))
getDefaultValue DefaultInputValue {defaultInputValue} = (Just defaultInputValue, Nothing)
getDefaultValue (FieldArgs args) = (Nothing, Just args)
