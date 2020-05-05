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

module Data.Morpheus.Types.Internal.Validation.Validator
  ( Validator(..)
  , SelectionValidator
  , InputValidator
  , BaseValidator
  , runValidator
  , askSchema
  , askContext
  , askFragments
  , Constraint(..)
  , withScope
  , withScopeType
  , withScopePosition
  , askScopeTypeName
  , askScopePosition
  , withInputScope
  , inputMessagePrefix
  , Context(..)
  , InputSource(..)
  , InputContext(..)
  , SelectionContext(..)
  , renderInputPrefix
  , Target(..)
  , Prop(..)
  , Resolution
  )
  where

import           Data.Semigroup                 ( (<>)
                                                , Semigroup(..)
                                                )
import         Control.Monad.Trans.Reader       ( ReaderT(..)
                                                , ask
                                                , withReaderT
                                                )
import           Data.Text                      ( intercalate )
import           Control.Monad.Trans.Class      ( MonadTrans(..) )


-- MORPHEUS
import           Data.Morpheus.Types.Internal.Operation
                                                ( Failure(..)
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Eventless )
import           Data.Morpheus.Types.Internal.AST
                                                ( Name
                                                , Position
                                                , Message
                                                , GQLErrors
                                                , GQLError(..)
                                                , Fragments
                                                , Schema
                                                , FieldsDefinition(..)
                                                , TypeDefinition(..)
                                                , Argument(..)
                                                , Variable(..)
                                                , VariableDefinitions
                                                , RESOLVED
                                                , RAW
                                                , VALID
                                                )

data Prop =
  Prop
    { propName  :: Name
    , propTypeName :: Name
    } deriving (Show)

type Path = [Prop]

renderPath :: Path -> Message
renderPath []    = ""
renderPath path = "in field \"" <> intercalate "." (fmap propName path) <> "\": "

renderInputPrefix :: InputContext -> Message
renderInputPrefix InputContext { inputPath , inputSource } = 
  renderSource inputSource <> renderPath inputPath

renderSource :: InputSource -> Message
renderSource (SourceArgument Argument { argumentName }) 
  = "Argument \"" <> argumentName <>"\" got invalid value. "
renderSource (SourceVariable Variable { variableName })
  = "Variable \"$" <> variableName <>"\" got invalid value. "

data Context = Context 
  { schema             :: Schema
  , fragments          :: Fragments
  , scopePosition      :: Position
  , scopeTypeName      :: Name
  , operationName      :: Maybe Name
  , scopeSelectionName :: Name
  } deriving (Show)

data InputContext 
  = InputContext 
    { inputSource :: InputSource
    , inputPath  :: [Prop]
    } deriving (Show)

data InputSource
  = SourceArgument (Argument RESOLVED)
  | SourceVariable (Variable RAW)
  deriving (Show)

newtype SelectionContext 
  = SelectionContext 
    { variables :: VariableDefinitions VALID
    } deriving (Show)

data Target 
  = TARGET_OBJECT 
  | TARGET_INPUT

data Constraint (a :: Target) where
  OBJECT :: Constraint 'TARGET_OBJECT
  INPUT  :: Constraint 'TARGET_INPUT
--  UNION  :: Constraint 'TARGET_UNION

type family Resolution (a :: Target)
type instance Resolution 'TARGET_OBJECT = (Name, FieldsDefinition)
type instance Resolution 'TARGET_INPUT = TypeDefinition
--type instance Resolution 'TARGET_UNION = DataUnion
 
withInputScope :: Prop -> InputValidator a -> InputValidator a
withInputScope prop = setContext update
  where
    update ctx@InputContext { inputPath = old } 
      = ctx { inputPath = old <> [prop] }


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


runValidator :: Validator ctx a -> Context -> ctx -> Eventless a
runValidator (Validator x) globalCTX ctx = runReaderT x (globalCTX,ctx) 

newtype Validator ctx a 
  = Validator 
    {
      _runValidator :: ReaderT 
          (Context, ctx) 
          Eventless
          a
    }
    deriving 
      ( Functor
      , Applicative
      , Monad
      )

type BaseValidator = Validator ()
type SelectionValidator = Validator SelectionContext
type InputValidator = Validator InputContext

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