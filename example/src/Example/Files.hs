module Example.Files
    ( getJson
    )
where

import           Data.Aeson                     ( eitherDecode
                                                , FromJSON
                                                )
import qualified Data.ByteString.Lazy          as L
                                                ( readFile )

jsonPath :: String -> String
jsonPath name = "example/data/" ++ name ++ ".json"

getJson :: FromJSON a => FilePath -> IO (Either String a)
getJson path = eitherDecode <$> L.readFile (jsonPath path)

