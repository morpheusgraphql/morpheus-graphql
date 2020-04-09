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
                                                , ObjectEntry(..)
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Stateless 
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

withObject :: (ValidObject -> Stateless a) -> ValidValue -> Stateless a
withObject f (Object object) = f object
withObject _ isType          = internalTypeMismatch "Object" isType

withMaybe :: Monad m => (ValidValue -> m a) -> ValidValue -> m (Maybe a)
withMaybe _      Null = pure Nothing
withMaybe decode x    = Just <$> decode x

withList :: (ValidValue -> Stateless a) -> ValidValue -> Stateless [a]
withList decode (List li) = traverse decode li
withList _      isType    = internalTypeMismatch "List" isType

withEnum :: (Key -> Stateless a) -> ValidValue -> Stateless a
withEnum decode (Enum value) = decode value
withEnum _      isType       = internalTypeMismatch "Enum" isType

withUnion :: (Key -> ValidObject -> ValidObject -> Stateless a) -> ValidObject -> Stateless a
withUnion decoder unions = do 
  (enum :: ValidValue) <- entryValue <$> selectBy ("__typename not found on Input Union" :: Message) "__typename" unions
  case enum of 
    (Enum key) -> selectOr notfound onFound key unions
      where 
        notfound = withObject (decoder key unions) (Object empty)
        onFound = withObject (decoder key unions) . entryValue
    _  -> failure ("__typename must be Enum" :: Message)

decodeFieldWith :: (ValidValue -> Stateless a) -> Key -> ValidObject -> Stateless a
decodeFieldWith decoder = selectOr (decoder Null) (decoder . entryValue)