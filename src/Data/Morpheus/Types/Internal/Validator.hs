{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}

module Data.Morpheus.Types.Internal.Validator
  ( Validator
  , ValidationContext(..)
  , runValidation
  , mapError
  , askSchema
  , askContext
  , askFragments
  , askFieldType
  , askTypeMember
  , selectRequired
  , selectKnown
  , lookupInputType
  , Constraint(..)
  , constraint
  , withScope
  , withScopeType
  , withScopePosition
  , askScopeTypeName
  , selectWithDefaultValue
  )
  where

import           Control.Monad                  ((>=>))
import           Control.Monad.Fail             ( MonadFail(..) )
import           Control.Monad.Trans.Class      ( MonadTrans(..) )
import           Data.Text                      ( pack )
import           Data.Semigroup                 ( (<>)
                                                , Semigroup(..)
                                                )
import         Control.Monad.Trans.Reader       ( ReaderT(..)
                                                , ask
                                                , withReaderT
                                                )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Operation
                                                ( Failure(..) 
                                                , Selectable
                                                , selectBy
                                                , selectOr
                                                , KeyOf(..)
                                                )
import qualified Data.Morpheus.Types.Internal.Resolving.Core as C
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Stateless )
import           Data.Morpheus.Types.Internal.AST
                                                ( Name
                                                , Position
                                                , Message
                                                , Ref(..)
                                                , TypeRef(..)
                                                , GQLErrors
                                                , GQLError(..)
                                                , Fragments
                                                , Schema
                                                , ValidationContext(..)
                                                , FieldDefinition(..)
                                                , FieldsDefinition(..)
                                                , TypeDefinition(..)
                                                , TypeContent(..)
                                                , isInputDataType
                                                , isFieldNullable
                                                )
import           Data.Morpheus.Error.ErrorClass ( MissingRequired(..)
                                                , KindViolation(..)
                                                , Unknown(..)
                                                , InternalError(..)
                                                , Target(..)
                                                )


data Constraint (a :: Target) where
  OBJECT :: Constraint 'TARGET_OBJECT
  INPUT  :: Constraint 'TARGET_INPUT
--  UNION  :: Constraint 'TARGET_UNION

type family Resolution (a :: Target)
type instance Resolution 'TARGET_OBJECT = (Name, FieldsDefinition)
type instance Resolution 'TARGET_INPUT = TypeDefinition
--type instance Resolution 'TARGET_UNION = DataUnion

constraint 
  :: forall (a :: Target) ctx. KindViolation a ctx 
  => Constraint ( a :: Target) 
  -> ctx 
  -> TypeDefinition 
  -> Validator (Resolution a)
constraint OBJECT  _   TypeDefinition { typeContent = DataObject { objectFields } , typeName } 
  = pure (typeName, objectFields)
-- constraint UNION   _   TypeDefinition { typeContent = DataUnion members } = pure members
constraint INPUT   _   x | isInputDataType x = pure x 
constraint target  ctx _  = failure [kindViolation target ctx]

lookupInputType 
  :: Failure e Validator 
  => Name 
  -> e 
  -> Validator TypeDefinition
lookupInputType name errors 
  = askSchema
    >>= selectBy errors name 
    >>= input
  where
    input x | isInputDataType x = pure x
            | otherwise       = failure errors

selectRequired 
  ::  ( Selectable c value
      , MissingRequired c
      ) 
  => Ref 
  -> c 
  -> Validator value
selectRequired selector container 
  = do 
    ctx <- askContext
    selectBy
      [missingRequired ctx selector container] 
      (keyOf selector) 
      container

-- isNull :: a -> Bool
-- isNull = const False

selectWithDefaultValue 
  ::  ( Selectable values value
      , MissingRequired values
      )
  => value
  -> FieldDefinition
  -> values
  -> Validator value
selectWithDefaultValue 
  fallbackValue 
  field@FieldDefinition { fieldName }
  values
  = selectOr 
    handleNullable 
    pure
    fieldName
    values 
  where
    ------------------
    handleNullable
      | isFieldNullable field = pure fallbackValue
      | otherwise             = failSelection
    -----------------
    failSelection = do
        ctx <- askContext
        failure [missingRequired ctx (Ref fieldName (scopePosition ctx)) values]

selectKnown 
  ::  ( Selectable c a
      , Unknown c
      , KeyOf (UnknownSelector c)
      ) 
  => UnknownSelector c 
  -> c 
  -> Validator a
selectKnown selector lib  
  = do 
    ctx <- askContext
    selectBy
      (unknown ctx lib selector) 
      (keyOf selector)  
      lib

askFieldType
  :: FieldDefinition
  -> Validator TypeDefinition
askFieldType field@FieldDefinition{ fieldType = TypeRef { typeConName }  }
  = do
    schema <- askSchema
    selectBy
        [internalError field] 
        typeConName 
        schema

askTypeMember
  :: Name
  -> Validator (Name, FieldsDefinition)
askTypeMember 
  name
  = askSchema
      >>= selectOr notFound pure name 
      >>= constraintOBJECT 
    where 
      notFound = do
          scopeType <- askScopeTypeName
          failure $
              "Type \"" <> name
              <> "\" referenced by union \"" <> scopeType 
              <> "\" can't found in Schema."
      --------------------------------------
      constraintOBJECT TypeDefinition { typeName , typeContent } = con typeContent
        where
          con DataObject { objectFields } = pure (typeName, objectFields)
          con _ = do 
            scopeType <- askScopeTypeName
            failure $
                "Type \"" <> typeName
                  <> "\" referenced by union \"" <> scopeType 
                  <> "\" must be an OBJECT."

runValidation :: Validator a -> ValidationContext -> Stateless a
runValidation (Validator x) = runReaderT x 

mapError 
  :: (GQLError -> GQLError)
  -> Validator a
  -> Validator a
mapError f (Validator x) = Validator $ ReaderT $ C.mapError f . runReaderT x 

askContext :: Validator ValidationContext
askContext = Validator ask

askSchema :: Validator Schema
askSchema = schema <$> askContext
   
askFragments :: Validator Fragments
askFragments = fragments <$> askContext

askScopeTypeName :: Validator Name
askScopeTypeName = scopeTypeName <$> askContext

setContext 
  :: (ValidationContext -> ValidationContext) 
  -> Validator a 
  -> Validator a
setContext update = Validator . withReaderT update . _runValidation

withScope :: Name -> Position -> Validator a -> Validator a
withScope scopeTypeName scopePosition = setContext update
     where
       update ctx = ctx { scopeTypeName , scopePosition }


withScopePosition :: Position -> Validator a -> Validator a
withScopePosition scopePosition = setContext update
    where
      update ctx = ctx { scopePosition  }

withScopeType :: Name -> Validator a -> Validator a
withScopeType scopeTypeName = Validator . withReaderT update . _runValidation
    where
      update ctx = ctx { scopeTypeName  }

newtype Validator a 
  = Validator 
    {
      _runValidation :: ReaderT 
          ValidationContext 
          Stateless
          a
    }
    deriving 
      ( Functor
      , Applicative
      , Monad
      )

instance MonadFail Validator where 
  fail = failure . pack

instance Failure Message Validator where
  failure inputMessage = do 
    position <- scopePosition <$> askContext 
    failure 
      [
        GQLError 
          { message = "INTERNAL: " <> inputMessage
          , locations = [position]
          }
      ]

instance Failure GQLErrors Validator where
  failure = Validator . lift . failure