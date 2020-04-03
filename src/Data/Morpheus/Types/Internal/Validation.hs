{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.Morpheus.Types.Internal.Validation
  ( Validation
  , ValidationContext(..)
  , runValidation
  , mapError
  , askSchema
  , askContext
  , askFragments
  )
  where

import           Control.Monad.Fail             ( MonadFail(..) )
import           Control.Monad.Trans.Class      ( MonadTrans(..) )
-- import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Text                      ( pack )
import           Data.Semigroup                 ( (<>)
                                                , Semigroup(..)
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT(..)
                                                , ask
                                                , mapReaderT
                                                , withReaderT
                                                , runReaderT
                                                )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Operation
                                                ( Failure(..) )
import qualified Data.Morpheus.Types.Internal.Resolving.Core as C
import           Data.Morpheus.Types.Internal.Resolving.Core
                                                ( Stateless
                                                )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Name
                                                , Message
                                                , Position
                                                , GQLErrors
                                                , GQLError(..)
                                                )
import           Data.Morpheus.Types.Internal.AST.Data
                                                ( Schema
                                                )
import           Data.Morpheus.Types.Internal.AST.Selection
                                                ( Fragments
                                                )

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

data ValidationContext 
  = ValidationContext 
    { schema          :: Schema
    , fragments       :: Fragments
    , operationName   :: Maybe Name
    , scopePosition   :: Position
      --operation :: Operation RAW
    } 
    deriving (Show)

newtype Validation a 
  = Validation 
    {
      runV :: ReaderT 
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
