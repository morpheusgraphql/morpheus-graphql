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
{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Morpheus.Types.Internal.Validator
  ( Validator
  , SelectionValidator
  , InputValidator
  , BaseValidator
  , runValidator
  , askSchema
  , askContext
  , askFragments
  , askFieldType
  , askTypeMember
  , selectRequired
  , selectKnown
  , Constraint(..)
  , constraint
  , withScope
  , withScopeType
  , withScopePosition
  , askScopeTypeName
  , selectWithDefaultValue
  , askScopePosition
  , askInputFieldType
  , askInputMember
  , startInput
  , withInputScope
  , inputMessagePrefix
  )
  where

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
                                                , SelectionContext(..)
                                                , FieldDefinition(..)
                                                , FieldsDefinition(..)
                                                , TypeDefinition(..)
                                                , TypeContent(..)
                                                , isInputDataType
                                                , isFieldNullable
                                                , InputSource(..)
                                                , InputContext(..)
                                                , Context(..)
                                                , Prop(..)
                                                , renderInputPrefix
                                                )
import           Data.Morpheus.Error.ErrorClass ( MissingRequired(..)
                                                , KindViolation(..)
                                                , Unknown(..)
                                                , InternalError(..)
                                                , Target(..)
                                                , CTX
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
  -> SelectionValidator (Resolution a)
constraint OBJECT  _   TypeDefinition { typeContent = DataObject { objectFields } , typeName } 
  = pure (typeName, objectFields)
-- constraint UNION   _   TypeDefinition { typeContent = DataUnion members } = pure members
constraint INPUT   _   x | isInputDataType x = pure x 
constraint target  ctx _  = failure [kindViolation target ctx]

selectRequired 
  ::  ( Selectable c value
      , MissingRequired c
      ) 
  => Ref 
  -> c
  -> Validator (CTX c) value
selectRequired selector container 
  = do 
    (gctx,ctx) <- Validator ask
    selectBy
      [missingRequired gctx ctx selector container] 
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
  -> Validator (CTX values) value
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
        (gctx, ctx) <- Validator ask
        failure [missingRequired gctx ctx (Ref fieldName (scopePosition gctx)) values]

selectKnown 
  ::  ( Selectable c a
      , Unknown c
      , KeyOf (UnknownSelector c)
      ) 
  => UnknownSelector c 
  -> c 
  -> Validator (CTX c) a
selectKnown selector lib  
  = do 
    (gctx, ctx) <- Validator ask
    selectBy
      (unknown gctx ctx lib selector) 
      (keyOf selector)  
      lib

askFieldType
  :: FieldDefinition
  -> SelectionValidator TypeDefinition
askFieldType field@FieldDefinition{ fieldType = TypeRef { typeConName }  }
  = do
    schema <- askSchema
    selectBy
        [internalError field] 
        typeConName 
        schema

askTypeMember
  :: Name
  -> SelectionValidator (Name, FieldsDefinition)
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

askInputFieldType
  :: FieldDefinition
  -> InputValidator TypeDefinition
askInputFieldType field@FieldDefinition{ fieldName , fieldType = TypeRef { typeConName }  }
  = askSchema
    >>= selectBy
        [internalError field] 
        typeConName 
    >>= constraintINPUT
 where
  constraintINPUT x 
    | isInputDataType x = pure x
    | otherwise         = failure $
        "Type \"" <> typeName x
        <> "\" referenced by field \"" <> fieldName
        <> "\" must be an input type."

askInputMember
  :: Name
  -> InputValidator TypeDefinition
askInputMember 
  name
  = askSchema
      >>= selectOr notFound pure name 
      >>= constraintINPUT_OBJECT 
    where 
      typeInfo tName
        = "Type \"" <> tName <> "\" referenced by inputUnion " 
      notFound = do
          scopeType <- askScopeTypeName
          failure $ typeInfo name <> scopeType <> "\" can't found in Schema."
      --------------------------------------
      constraintINPUT_OBJECT tyDef@TypeDefinition { typeName , typeContent } = con typeContent
        where
          con DataInputObject { } = pure tyDef
          con _ = do 
            scopeType <- askScopeTypeName
            failure $ typeInfo typeName <> "\"" <> scopeType <> "\" must be an INPUT_OBJECT."

startInput :: InputSource -> InputValidator a -> SelectionValidator a
startInput inputSource 
  = setContext 
  $ const InputContext 
    { inputSource 
    , inputPath = [] 
    }
 
withInputScope :: Prop -> InputValidator a -> InputValidator a
withInputScope prop = setContext update
  where
    update ctx@InputContext { inputPath = old } 
      = ctx { inputPath = old <> [prop] }

runValidator :: Validator ctx a -> Context -> ctx -> Stateless a
runValidator (Validator x) globalCTX ctx = runReaderT x (globalCTX,ctx) 

askContext :: Validator ctx ctx
askContext = snd <$> Validator ask

askSchema :: Validator ctx Schema
askSchema = schema . fst <$> Validator ask
   
askFragments :: Validator ctx Fragments
askFragments = fragments . fst <$> Validator ask

askScopeTypeName :: Validator ctx  Name
askScopeTypeName = scopeTypeName . fst <$> Validator ask

askScopePosition :: Validator ctx Position
askScopePosition = scopePosition . fst <$> Validator ask

setContext 
  :: (c' -> c) 
  -> Validator c a 
  -> Validator c' a
setContext f = Validator . withReaderT ( \(x,y) -> (x,f y)) . _runValidator

setGlobalContext 
  :: (Context -> Context) 
  -> Validator c a 
  -> Validator c a
setGlobalContext f = Validator . withReaderT ( \(x,y) -> (f x,y)) . _runValidator


withScope :: Name -> Position -> Validator ctx a -> Validator ctx a
withScope scopeTypeName scopePosition = setGlobalContext update
     where
       update ctx = ctx { scopeTypeName , scopePosition }


withScopePosition :: Position -> Validator ctx a -> Validator ctx a
withScopePosition scopePosition = setGlobalContext update
    where
      update ctx = ctx { scopePosition  }

withScopeType :: Name -> Validator ctx a -> Validator ctx a
withScopeType scopeTypeName = setGlobalContext update
    where
      update ctx = ctx { scopeTypeName  }

inputMessagePrefix :: InputValidator Message 
inputMessagePrefix = renderInputPrefix <$> askContext

newtype Validator ctx a 
  = Validator 
    {
      _runValidator :: ReaderT 
          (Context, ctx) 
          Stateless
          a
    }
    deriving 
      ( Functor
      , Applicative
      , Monad
      )

type BaseValidator = Validator SelectionContext
type SelectionValidator = Validator SelectionContext
type InputValidator = Validator InputContext

-- instance MonadFail (SelectionValidator ctx) where 
--  fail = failure . pack

-- can be only used for internal errors
instance Failure Message (Validator ctx) where
  failure inputMessage = do 
    position <- askScopePosition
    failure 
      [
        GQLError 
          { message = "INTERNAL: " <> inputMessage
          , locations = [position]
          }
      ]

instance Failure GQLErrors (Validator ctx) where
  failure = Validator . lift . failure