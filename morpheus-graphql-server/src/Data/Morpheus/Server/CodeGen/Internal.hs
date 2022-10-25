module Data.Morpheus.Server.CodeGen.Internal
  ( Typeable,
    Text,
    Generic,
  )
where

import Data.Data (Typeable)
-- add type class instances
import Data.Morpheus.Server.Deriving.App ()
import Data.Text (Text)
import GHC.Generics (Generic)
