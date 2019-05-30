module Data.Morpheus
  ( interpreter
  , streamInterpreter
  , packStream
  ) where

import           Data.Morpheus.Interpreter       (Interpreter (..))
import           Data.Morpheus.StreamInterpreter (packStream, streamInterpreter)