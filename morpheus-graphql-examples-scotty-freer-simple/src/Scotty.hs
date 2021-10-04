{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Scotty (routes) where

import           API                       (api)
import           Control.Monad.Freer       (Eff, LastMember, Member)
import           Control.Monad.Trans.Class (lift)
import           Data.Typeable             (Typeable)
import           DeityRepo                 (DeityRepo)
import           Web.Scotty.Trans          (ScottyError, ScottyT, body, post,
                                            raw)

routes
  :: ( ScottyError e
     , Typeable effs
     , Member DeityRepo effs
     , LastMember IO effs
     )
  => ScottyT e (Eff effs) ()
routes = do
  post "/graphql" $ body >>= lift . api >>= raw
