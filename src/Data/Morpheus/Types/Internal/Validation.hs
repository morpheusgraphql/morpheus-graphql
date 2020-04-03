{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}

module Data.Morpheus.Types.Internal.Validation
  ( Validation
  , ValidationContext(..)
  , runValidation
  , mapError
  , askSchema
  , askContext
  , askFragments
  , selectRequired
  )
  where

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
                                                , Ref
                                                , GQLErrors
                                                , GQLError(..)
                                                , Fragments
                                                , Schema
                                                , ValidationContext(..)
                                                )
import           Data.Morpheus.Error.ErrorClass ( MissingRequired(..)
                                                )

selectRequired 
  ::  ( Selectable c value
      , MissingRequired c
      --, KeyOf sel
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
