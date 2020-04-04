{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}


module Data.Morpheus.Types.Internal.Validation
  ( Validation
  , ValidationContext(..)
  , runValidation
  , mapError
  , askSchema
  , askContext
  , askFragments
  , selectRequired
  , selectKnown
  --, constraintInput
  , lookupUnionTypes
  , lookupFieldAsSelectionSet
  , lookupInputType
  , lookupSelectionField
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
                                                , Position
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
                                                )
--import           Data.Morpheus.Error.Utils      (errorMessage)
import           Data.Morpheus.Error.Selection  ( cannotQueryField
                                                , hasNoSubfields
                                                )

data Target 
  = TARGET_OBJECT 
  | TARGET_INPUT

data Constraint (a :: Target) where
  OBJECT :: Constraint 'TARGET_OBJECT
  INPUT  :: Constraint 'TARGET_INPUT

type family Resolution (a :: Target)
type instance Resolution 'TARGET_OBJECT = (Name, FieldsDefinition)
type instance Resolution 'TARGET_INPUT = TypeDefinition

-- -- or
-- data Resolution (a :: Target) where
  -- TypeObject :: 
  --   { name :: Name
  --   , fields :: FieldsDefinition 
  --   } -> Resolution 'OBJECT_TARGET
  -- TypeInput :: 
  --   { unpackInput :: TypeDefinition 
  --   } -> Resolution 'INPUT_TARGET

constraint 
  :: KindViolation ctx 
  => Constraint a 
  -> ctx 
  -> TypeDefinition 
  -> Validation (Resolution a)
constraint OBJECT _ TypeDefinition { typeContent = DataObject { objectFields } , typeName } 
  = pure (typeName, objectFields)
constraint OBJECT ctx _ = failure [kindViolation ctx]
constraint INPUT ctx x = orFail (isInputDataType x) [kindViolation ctx] x

constraintObject2 
  :: Failure error m 
  => error 
  -> TypeDefinition 
  -> m (Name, FieldsDefinition)
constraintObject2 
  _ 
  TypeDefinition 
    { typeContent = DataObject { objectFields } 
    , typeName 
    } 
  = pure (typeName, objectFields)
constraintObject2 err _ = failure err

constraintUnion :: Failure error Validation => error -> TypeDefinition -> Validation DataUnion
constraintUnion _ TypeDefinition { typeContent = DataUnion members } = pure members
constraintUnion err _ = failure err

lookupFieldAsSelectionSet
  :: (Monad m, Failure GQLErrors m)
  => Ref
  -> Schema
  -> FieldDefinition  
  -> m (Name, FieldsDefinition )
lookupFieldAsSelectionSet 
  ref 
  schema 
  FieldDefinition { fieldType = TypeRef { typeConName } }
  = selectBy err typeConName schema 
    >>= constraintObject2 err
  where err = hasNoSubfields ref typeConName

lookupSelectionField
  :: (Monad m , Failure GQLErrors m)
  => Position
  -> Name
  -> (Name, FieldsDefinition)
  -> m FieldDefinition
lookupSelectionField position fieldName (typeName, field) 
  = selectBy err fieldName field
    where err = cannotQueryField fieldName typeName position

lookupInputType 
  :: Failure e Validation 
  => Name 
  -> Schema 
  -> e 
  -> Validation TypeDefinition
lookupInputType name lib errors = selectBy errors name lib >>= input
  where
    input x | isInputDataType x = pure x
            | otherwise       = failure errors


-- get union Types defined in GraphQL schema -> (union Tag, union Selection set)
-- for example 
-- User | Admin | Product
lookupUnionTypes
  :: Ref
  -> Schema
  -> FieldDefinition 
  -> Validation [(Name, FieldsDefinition)]
lookupUnionTypes 
  ref 
  schema 
  FieldDefinition { fieldType = TypeRef { typeConName  } }
  = selectKnown (ref { refName = typeConName }) schema 
    >>= constraintUnion err
    >>= traverse (
          (\name -> selectKnown (ref { refName = name}) schema) 
          >=> constraintObject2 err
        )
  where 
    err = hasNoSubfields ref typeConName

orFail 
  :: (Monad m, Failure e m) 
  => Bool
  -> e
  -> a
  -> m a
orFail cond err x
      | cond = pure x
      | otherwise = failure err

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
