module Cookhouse.Middlewares.CORS
  ( corsMiddleware
  ) where

import Data.List
import Data.String

import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai

import Cookhouse.Config

corsMiddleware :: Config -> Middleware
corsMiddleware config app =
  if null (configCORSOrigins config)
    then app
    else let corsHeaders = mkCorsHeaders config
         in \req f ->
           if requestMethod req == methodOptions
             then f $ responseLBS status200 corsHeaders ""
             else app req f

mkCorsHeaders :: (IsString a, IsString b) => Config -> [(a, b)]
mkCorsHeaders config = case configCORSOrigins config of
  []   -> []
  urls ->
    let allowOrigin  = ( "Access-Control-Allow-Origin"
                       , fromString $ intercalate " " $ map show urls
                       )
        allowHeaders = ( "Access-Control-Allow-Headers"
                       , fromString $ "Origin, Host, Referer, User-Agent, "
                           ++ "Connection, Content-Type, Accept, X-API-Token"
                       )
        allowMethods = ( "Access-Control-Allow-Methods"
                       , "GET, POST, PUT, OPTIONS, DELETE"
                       )
    in [allowOrigin, allowHeaders, allowMethods]
