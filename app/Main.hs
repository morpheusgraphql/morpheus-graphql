module Main
    ( main
    )
where

import           Example.Handler                ( handler )
import           AWSLambda.Events.APIGateway    ( apiGatewayMain )

main = apiGatewayMain handler
