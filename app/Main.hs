module Main ( main ) where

import AWSLambda.Events.APIGateway (apiGatewayMain)
import Example.Handler             (handler)

main = apiGatewayMain handler
