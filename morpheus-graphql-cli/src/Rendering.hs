module Rendering
  ( toMorpheusHaskellAPi
  )
where

import           Rendering.Render               ( renderHaskellDocument )
import           Data.Morpheus.Document         ( parseDSL )
import           Data.ByteString.Lazy.Char8     ( ByteString
                                                , pack
                                                )

toMorpheusHaskellAPi :: String -> ByteString -> Either ByteString ByteString
toMorpheusHaskellAPi moduleName doc = case parseDSL doc of
  Left  errors -> Left $ pack errors
  Right lib    -> Right $ renderHaskellDocument moduleName lib
