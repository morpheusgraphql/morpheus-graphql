module Files
  ( getJson
  )
where

import           Data.Aeson                     ( FromJSON
                                                , eitherDecode
                                                )
import qualified Data.ByteString.Lazy          as L
                                                ( readFile )

jsonPath :: String -> String
jsonPath name = "examples/db/" ++ name ++ ".json"

getJson :: FromJSON a => FilePath -> IO (Either String a)
getJson path = eitherDecode <$> L.readFile (jsonPath path)
