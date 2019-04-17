module Data.Morpheus.PreProcess.Resolve.Arguments
  ( resolveArguments
  ) where

import           Data.Morpheus.PreProcess.Variable      (resolveArgumentValue)
import           Data.Morpheus.Types.Error              (Validation)
import qualified Data.Morpheus.Types.Query.RawSelection as Raw (RawArguments)
import           Data.Morpheus.Types.Query.Selection    (Arguments)
import           Data.Morpheus.Types.Types              (Variables)
import           Data.Morpheus.Schema.Internal.Types    (GObject (..), ObjectField (..))

resolveArguments :: Variables -> [(Text, InputField)] -> Raw.RawArguments -> Validation Arguments
resolveArguments variables' _ = mapM (resolveArgumentValue variables')
