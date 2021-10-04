{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module API (api) where

import           Control.Monad.Freer        (Eff, Member)
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.FileEmbed             (makeRelativeToProject)
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     (importGQLDocument)
import           Data.Morpheus.Types        (Arg (Arg), RootResolver (..),
                                             Undefined (..), liftEither)
import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           DeityRepo                  (DeityRepo, getDeityByName)
import           Prelude                    hiding (error)
import qualified Types                      as T

importGQLDocument =<< makeRelativeToProject "src/api.gql"

api :: (Member DeityRepo effs, Typeable effs) => ByteString -> Eff effs ByteString
api = interpreter rootResolver

rootResolver :: Member DeityRepo effs => RootResolver (Eff effs) () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {deity = deityResolver},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  where
    deityResolver (Arg name) =
      liftEither $ toResponse <$> getDeityByName name

toResponse
  :: (Applicative m, Show a)
  => Either a T.Deity -> Either String (Deity m)
toResponse (Right deity) = Right $ toResponse' deity
toResponse (Left error)  = Left $ show error

toResponse' :: Applicative m => T.Deity -> Deity m
toResponse' (T.Deity name power) = Deity { name = pure name, power = pure power }
