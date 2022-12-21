module Domains.API
  ( domainsApp,
  )
where

import Data.Data (Typeable)
import Data.Morpheus (App)
import Data.Semigroup ((<>))
import qualified Domains.Posts.Resolver as P
import qualified Domains.Users.Resolver as U
import Namespaces.API ()
import Operation.API ()
import Prelude (Monad)

domainsApp :: (Typeable m, Monad m) => App () m
domainsApp = P.app <> U.app
