module Example.Files
    ( getJson
    )
where

import           Data.Aeson                     ( eitherDecode
                                                , FromJSON
                                                )
import           Data.ByteString.Lazy           ( readFile )
import           Prelude                 hiding ( readFile )

jsonPath :: String -> String
jsonPath name = "example-data/" ++ name ++ ".json"

getJson :: FromJSON a => FilePath -> IO (Either String a)
getJson path = eitherDecode <$> readFile (jsonPath path)

