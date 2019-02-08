{-# LANGUAGE OverloadedStrings #-}

module Example.Handler
    ( handler
    )
where

import           AWSLambda.Events.APIGateway    ( APIGatewayProxyRequest
                                                , APIGatewayProxyResponse
                                                , responseOK
                                                , responseBodyEmbedded
                                                , requestBodyEmbedded
                                                )
import           Data.Aeson.Embedded            ( Embedded )
import           Control.Lens                   ( (^.)
                                                , (&)
                                                , (?~)
                                                )
import           Example.Schema                 ( gqlHandler )
import           Data.MorpheusGraphQL           ( GQLRequest(..)
                                                , GQLResponce
                                                )
import           Data.Maybe                     ( fromMaybe )


toResponce :: a -> APIGatewayProxyResponse (Embedded a)
toResponce obj = responseOK & responseBodyEmbedded ?~ obj

toQuery :: APIGatewayProxyRequest (Embedded GQLRequest) -> GQLRequest
toQuery request =
    fromMaybe (GQLRequest "" Nothing Nothing) $ request ^. requestBodyEmbedded

handler
    :: APIGatewayProxyRequest (Embedded GQLRequest)
    -> IO (APIGatewayProxyResponse (Embedded GQLResponce))
handler inputString = toResponce <$> (gqlHandler $ toQuery inputString)
