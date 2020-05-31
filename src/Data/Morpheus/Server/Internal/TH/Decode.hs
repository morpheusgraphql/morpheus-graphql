{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.Internal.TH.Decode
  ( withObject,
    withMaybe,
    withList,
    withEnum,
    withUnion,
    decodeFieldWith,
    decodeObjectExpQ,
  )
where

-- MORPHEUS
import Data.Morpheus.Error
  ( internalTypeMismatch,
  )
import Data.Morpheus.Internal.TH
  ( nameConE,
    nameVarE,
  )
import Data.Morpheus.Internal.Utils
  ( empty,
    selectBy,
    selectOr,
  )
import Data.Morpheus.Types.Internal.AST
  ( ConsD (..),
    FieldDefinition (..),
    FieldName,
    Message,
    Message,
    ObjectEntry (..),
    TypeName (..),
    ValidObject,
    ValidValue,
    Value (..),
    msg,
    toFieldName,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Failure (..),
  )
import Data.Semigroup ((<>))
import Language.Haskell.TH
  ( ExpQ,
    uInfixE,
    varE,
  )

decodeObjectExpQ :: ExpQ -> ConsD cat -> ExpQ
decodeObjectExpQ fieldDecoder ConsD {cName, cFields} = handleFields cFields
  where
    consName = nameConE cName
    ----------------------------------------------------------------------------------
    handleFields fNames = uInfixE consName (varE '(<$>)) (applyFields fNames)
      where
        applyFields [] = fail $ show ("No Empty fields on " <> msg cName :: Message)
        applyFields [x] = defField x
        applyFields (x : xs) = uInfixE (defField x) (varE '(<*>)) (applyFields xs)
        ------------------------------------------------------------------------
        defField FieldDefinition {fieldName} =
          uInfixE
            (nameVarE "o")
            fieldDecoder
            [|fieldName|]

withObject :: (ValidObject -> Eventless a) -> ValidValue -> Eventless a
withObject f (Object object) = f object
withObject _ isType = internalTypeMismatch "Object" isType

withMaybe :: Monad m => (ValidValue -> m a) -> ValidValue -> m (Maybe a)
withMaybe _ Null = pure Nothing
withMaybe decode x = Just <$> decode x

withList :: (ValidValue -> Eventless a) -> ValidValue -> Eventless [a]
withList decode (List li) = traverse decode li
withList _ isType = internalTypeMismatch "List" isType

withEnum :: (TypeName -> Eventless a) -> ValidValue -> Eventless a
withEnum decode (Enum value) = decode value
withEnum _ isType = internalTypeMismatch "Enum" isType

withUnion :: (TypeName -> ValidObject -> ValidObject -> Eventless a) -> ValidObject -> Eventless a
withUnion decoder unions = do
  (enum :: ValidValue) <- entryValue <$> selectBy ("__typename not found on Input Union" :: Message) "__typename" unions
  case enum of
    (Enum key) -> selectOr notfound onFound (toFieldName key) unions
      where
        notfound = withObject (decoder key unions) (Object empty)
        onFound = withObject (decoder key unions) . entryValue
    _ -> failure ("__typename must be Enum" :: Message)

decodeFieldWith :: (ValidValue -> Eventless a) -> FieldName -> ValidObject -> Eventless a
decodeFieldWith decoder = selectOr (decoder Null) (decoder . entryValue)
