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
import           Data.Morpheus                  ( GQLRequest(..)
                                                , GQLResponse
                                                )
import           Data.Maybe                     ( fromMaybe )


toResponse :: a -> APIGatewayProxyResponse (Embedded a)
toResponse obj = responseOK & responseBodyEmbedded ?~ obj

toQuery :: APIGatewayProxyRequest (Embedded GQLRequest) -> GQLRequest
toQuery request =
    fromMaybe (GQLRequest "" Nothing Nothing) $ request ^. requestBodyEmbedded

handler
    :: APIGatewayProxyRequest (Embedded GQLRequest)
    -> IO (APIGatewayProxyResponse (Embedded GQLResponse))
handler inputString = toResponse <$> (gqlHandler $ toQuery inputString)
