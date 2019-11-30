{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

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

import           Data.Semigroup                 ( (<>) )
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
                                                ( DataField(..)
                                                , Key
                                                , ConsD(..) 
                                                , Object
                                                , Value(..)
                                                , Message
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation, Failure(..) )


decodeObjectExpQ :: ExpQ -> ConsD -> ExpQ
decodeObjectExpQ fieldDecoder ConsD { cName, cFields } = handleFields cFields
 where
  consName = conE (mkName cName)
  ----------------------------------------------------------------------------------
  handleFields fNames = uInfixE consName (varE '(<$>)) (applyFields fNames)
   where
    applyFields []       = fail "No Empty fields"
    applyFields [x     ] = defField x
    applyFields (x : xs) = uInfixE (defField x) (varE '(<*>)) (applyFields xs)
    ------------------------------------------------------------------------
    defField DataField { fieldName } = uInfixE (varE (mkName "o"))
                                               fieldDecoder
                                               [|fName|]
      where fName = unpack fieldName

withObject :: (Object -> Validation a) -> Value -> Validation a
withObject f (Object object) = f object
withObject _ isType          = internalTypeMismatch "Object" isType

withMaybe :: Monad m => (Value -> m a) -> Value -> m (Maybe a)
withMaybe _      Null = pure Nothing
withMaybe decode x    = Just <$> decode x

withList :: (Value -> Validation a) -> Value -> Validation [a]
withList decode (List li) = mapM decode li
withList _      isType    = internalTypeMismatch "List" isType

withEnum :: (Key -> Validation a) -> Value -> Validation a
withEnum decode (Enum value) = decode value
withEnum _      isType       = internalTypeMismatch "Enum" isType

withUnion :: (Key -> Object -> Object -> Validation a) -> Object -> Validation a
withUnion decoder unions = case lookup "__typename" unions of
  Just (Enum key) -> case lookup key unions of
    Nothing -> withObject (decoder key unions) (Object [])
    Just value -> withObject (decoder key unions) value
  Just _  -> failure ("__typename must be Enum" :: Message)
  Nothing -> failure ("__typename not found on Input Union" :: Message)

decodeFieldWith :: (Value -> Validation a) -> Key -> Object -> Validation a
decodeFieldWith decoder name object = case lookup name object of
  Nothing    -> failure ("Missing Field: \"" <> name <> "\"")
  Just value -> decoder value