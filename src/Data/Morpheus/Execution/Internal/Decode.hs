{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Data.Morpheus.Execution.Internal.Decode
  ( withObject
  , withMaybe
  , withList
  , withEnum
  , withUnion
  , decodeFieldWith
  , decodeObjectExpQ
  )
where

import           Data.Text                      ( unpack )
import           Language.Haskell.TH            ( ExpQ
                                                , conE
                                                , mkName
                                                , uInfixE
                                                , varE
                                                )

-- MORPHEUS
import           Data.Morpheus.Error.Internal   ( 
                                                internalTypeMismatch
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( FieldDefinition(..)
                                                , Key
                                                , ConsD(..) 
                                                , ValidObject
                                                , Value(..)
                                                , ValidValue
                                                , Message
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation 
                                                , Failure(..) 
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                ( selectBy
                                                , selectOr
                                                , empty
                                                )

decodeObjectExpQ :: ExpQ -> ConsD -> ExpQ
decodeObjectExpQ fieldDecoder ConsD { cName, cFields } = handleFields cFields
 where
  consName = conE (mkName $ unpack cName)
  ----------------------------------------------------------------------------------
  handleFields fNames = uInfixE consName (varE '(<$>)) (applyFields fNames)
   where
    applyFields []       = fail "No Empty fields"
    applyFields [x     ] = defField x
    applyFields (x : xs) = uInfixE (defField x) (varE '(<*>)) (applyFields xs)
    ------------------------------------------------------------------------
    defField FieldDefinition { fieldName } = uInfixE (varE (mkName "o"))
                                               fieldDecoder
                                               [|fName|]
      where fName = unpack fieldName

withObject :: (ValidObject -> Validation a) -> ValidValue -> Validation a
withObject f (Object object) = f object
withObject _ isType          = internalTypeMismatch "Object" isType

withMaybe :: Monad m => (ValidValue -> m a) -> ValidValue -> m (Maybe a)
withMaybe _      Null = pure Nothing
withMaybe decode x    = Just <$> decode x

withList :: (ValidValue -> Validation a) -> ValidValue -> Validation [a]
withList decode (List li) = mapM decode li
withList _      isType    = internalTypeMismatch "List" isType

withEnum :: (Key -> Validation a) -> ValidValue -> Validation a
withEnum decode (Enum value) = decode value
withEnum _      isType       = internalTypeMismatch "Enum" isType

withUnion :: (Key -> ValidObject -> ValidObject -> Validation a) -> ValidObject -> Validation a
withUnion decoder unions = do 
  (enum :: ValidValue) <- selectBy ("__typename not found on Input Union" :: Message) "__typename" unions
  case enum of 
    (Enum key) -> selectOr notfound onFound key unions
      where 
        notfound = withObject (decoder key unions) (Object empty)
        onFound = withObject (decoder key unions)
    _  -> failure ("__typename must be Enum" :: Message)

decodeFieldWith :: (ValidValue -> Validation a) -> Key -> ValidObject -> Validation a
decodeFieldWith decoder = selectOr (decoder Null) decoder