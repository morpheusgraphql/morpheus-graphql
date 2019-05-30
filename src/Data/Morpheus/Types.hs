module Data.Morpheus.Types
  ( ScalarValue(..)
  , (::->)
  , (::->>)
  , Resolver(..)
  , ID(..)
  , GQLRoot(..)
  , withEffect
  ) where

import           Data.Morpheus.Types.ID             (ID (..))
import           Data.Morpheus.Types.Internal.Value (ScalarValue (..))
import           Data.Morpheus.Types.Resolver       ((::->), (::->>), Resolver (..), WithEffect (..))
import           Data.Morpheus.Types.Types          (GQLRoot (..))
import           Data.Text                          (Text)

withEffect :: [Text] -> Either String a -> Either String (WithEffect a)
withEffect channels v = WithEffect channels <$> v
