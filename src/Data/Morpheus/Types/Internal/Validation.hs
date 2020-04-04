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

module Data.Morpheus.Types.Internal.Validation
  ( Validation
  , ValidationContext(..)
  , runValidation
  , mapError
  , askSchema
  , askContext
  , askFragments
  , askFieldType
  , selectRequired
  , selectKnown
  , lookupUnionTypes
  , lookupInputType
  , Constraint(..)
  , constraint
  )
  where

import           Control.Monad                  ((>=>))
import           Control.Monad.Fail             ( MonadFail(..) )
import           Control.Monad.Trans.Class      ( MonadTrans(..) )
import           Data.Text                      ( pack )
import           Data.Semigroup                 ( (<>)
                                                , Semigroup(..)
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT(..)
                                                , ask
                                                )


-- MORPHEUS
import           Data.Morpheus.Types.Internal.Operation
                                                ( Failure(..) 
                                                , Selectable
                                                , selectBy
                                                , KeyOf(..)
                                                )
import qualified Data.Morpheus.Types.Internal.Resolving.Core as C
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Stateless )
import           Data.Morpheus.Types.Internal.AST
                                                ( Name
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
                                                , DataUnion
                                                , isInputDataType
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
  UNION  :: Constraint 'TARGET_UNION

type family Resolution (a :: Target)
type instance Resolution 'TARGET_OBJECT = (Name, FieldsDefinition)
type instance Resolution 'TARGET_INPUT = TypeDefinition
type instance Resolution 'TARGET_UNION = DataUnion

constraint 
  :: forall (a :: Target) ctx. KindViolation a ctx 
  => Constraint ( a :: Target) 
  -> ctx 
  -> TypeDefinition 
  -> Validation (Resolution a)
constraint OBJECT  _   TypeDefinition { typeContent = DataObject { objectFields } , typeName } 
  = pure (typeName, objectFields)
constraint UNION   _   TypeDefinition { typeContent = DataUnion members } = pure members
constraint INPUT   _   x | isInputDataType x = pure x 
constraint target  ctx _  = failure [kindViolation target ctx]

__constraint 
  :: forall (a :: Target) ctx. InternalError (Constraint a,ctx) 
  => Constraint ( a :: Target) 
  -> ctx 
  -> TypeDefinition 
  -> Validation (Resolution a)
__constraint OBJECT _ TypeDefinition { typeContent = DataObject { objectFields } , typeName } 
  = pure (typeName, objectFields)
__constraint UNION _ TypeDefinition { typeContent = DataUnion members } = pure members
__constraint INPUT _ x | isInputDataType x = pure x 
__constraint con ctx _  = failure [internalError (con,ctx) ]

lookupInputType 
  :: Failure e Validation 
  => Name 
  -> e 
  -> Validation TypeDefinition
lookupInputType name errors 
  = askSchema
    >>= selectBy errors name 
    >>= input
  where
    input x | isInputDataType x = pure x
            | otherwise       = failure errors

-- get union Types defined in GraphQL schema -> (union Tag, union Selection set)
-- for example 
-- User | Admin | Product
lookupUnionTypes
  :: Ref
  -> FieldDefinition 
  -> Validation [(Name, FieldsDefinition)]
lookupUnionTypes 
  ref 
  field@FieldDefinition { fieldType = TypeRef { typeConName  } }
  = askFieldType field
    >>= constraint UNION (ref,typeConName)
    >>= traverse 
          ( selectUnionType 
            >=> __constraint OBJECT (ref,typeConName)
          )
    where 
      selectUnionType name 
        = askSchema
          >>= selectKnown (ref { refName = name}) 
        -- TODO: internal UNIONerror 

selectRequired 
  ::  ( Selectable c value
      , MissingRequired c
      ) 
  => Ref 
  -> c 
  -> Validation value
selectRequired selector container 
  = do 
    ctx <- askContext
    selectBy
      [missingRequired ctx selector container] 
      (keyOf selector) 
      container

selectKnown 
  ::  ( Monad m
      , Failure GQLErrors m
      , Selectable c a
      , Unknown c
      , KeyOf (UnknownSelector c)
      ) 
  => UnknownSelector c 
  -> c 
  -> m a
selectKnown selector lib  
  = selectBy 
    (unknown lib selector) 
    (keyOf selector)  
    lib

askFieldType
  :: FieldDefinition
  -> Validation TypeDefinition
askFieldType field@FieldDefinition{ fieldType = TypeRef { typeConName }  }
  = do
    schema <- askSchema
    selectBy
        [internalError field] 
        typeConName 
        schema

runValidation :: Validation a -> ValidationContext -> Stateless a
runValidation (Validation x) = runReaderT x 

mapError 
  :: (GQLError -> GQLError)
  -> Validation a
  -> Validation a
mapError f (Validation x) = Validation $ ReaderT $ C.mapError f . runReaderT x 

askContext :: Validation ValidationContext
askContext = Validation ask

askSchema :: Validation Schema
askSchema = schema <$> askContext
   
askFragments :: Validation Fragments
askFragments = fragments <$> askContext

newtype Validation a 
  = Validation 
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

instance MonadFail Validation where 
  fail = failure . pack

instance Failure Message Validation where
  failure inputMessage = Validation $ do 
    position <- scopePosition <$> ask 
    lift
      $ failure 
      [
        GQLError 
          { message = "INTERNAL: " <> inputMessage
          , locations = [position]
          }
      ]

instance Failure GQLErrors Validation where
  failure = Validation . lift . failure