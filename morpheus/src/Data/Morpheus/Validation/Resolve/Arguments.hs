module Data.Morpheus.PreProcess.Resolve.Arguments
  ( resolveArguments
  ) where

import           Data.Morpheus.PreProcess.Variable      (resolveArgumentValue)
import           Data.Morpheus.Types.Error              (Validation)
import qualified Data.Morpheus.Types.Query.RawSelection as Raw (RawArguments)
import           Data.Morpheus.Types.Query.Selection    (Arguments)
import           Data.Morpheus.Types.Types              (Variables)

resolveArguments :: Variables -> Raw.RawArguments -> Validation Arguments
resolveArguments variables' = mapM (resolveArgumentValue variables')
