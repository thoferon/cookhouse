{-# LANGUAGE OverloadedStrings #-}

module Cookhouse.Middlewares.CORS
  ( corsMiddleware
  , setCorsHeaders
  ) where

import Data.List
import Data.String

import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai

import Web.Spock.Simple

import Cookhouse.Actions.Types
import Cookhouse.Config

corsMiddleware :: Config -> AppSpockM ()
corsMiddleware config = case configCORSOrigins config of
  [] -> return ()
  _  -> middleware $ \app req ->
    let corsHeaders = mkCorsHeaders config
    in if requestMethod req == methodOptions
         then ($ responseLBS status200 corsHeaders "")
         else app req

setCorsHeaders :: Config -> SpockAction a b c ()
setCorsHeaders = mapM_ (uncurry setHeader) . mkCorsHeaders

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
