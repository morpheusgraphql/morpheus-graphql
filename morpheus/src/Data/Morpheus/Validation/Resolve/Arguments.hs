module Data.Morpheus.Validation.Resolve.Arguments
  ( resolveArguments
  ) where

import           Data.Morpheus.Types.Error              (Validation)
import qualified Data.Morpheus.Types.Query.RawSelection as Raw (RawArguments)
import           Data.Morpheus.Types.Query.Selection    (Arguments)
import           Data.Morpheus.Types.Types              (Variables)
import           Data.Morpheus.Validation.Variable      (resolveArgumentValue)

resolveArguments :: Variables -> Raw.RawArguments -> Validation Arguments
resolveArguments variables' = mapM (resolveArgumentValue variables')
