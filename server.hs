{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO)
import Data.Monoid ((<>))
import qualified Data.ByteString as BS (ByteString, pack)
import Data.String (fromString)
import qualified Data.Text as T (Text, concat, pack)
import qualified Data.Text.Encoding as TE (encodeUtf8)
import Data.Maybe (mapMaybe, fromMaybe)
import Safe (lastMay)
import System.Environment (lookupEnv)
import Network.Wai (Application, Middleware, pathInfo)
import Network.Wai.Handler.Warp (run, defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettingsChain)
import Network.Wai.Application.Static (staticApp
                                      , defaultWebAppSettings
                                      , ssIndices
                                      , ssRedirectToIndex
                                      , ssAddTrailingSlash
                                      , ss404Handler
                                      , ssMaxAge
                                      )
import WaiAppStatic.Types (toPiece, MaxAge(MaxAgeSeconds))
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Middleware.ForceSSL (forceSSL)
import Network.Wai.Middleware.ForceDomain (forceDomain)
import Network.Wai.Middleware.Vhost (redirectTo)
import Network.Wai.Middleware.AddHeaders (addHeaders)

-- | The core application.
-- It serves files from `_site` whic is where Hakyll will place the generated
-- site.
staticSite :: Maybe String -> Application
staticSite path = staticApp
              (defaultWebAppSettings $ fromString $ fromMaybe "_site" path)
              { ssIndices  = mapMaybe (toPiece . T.pack) ["index.html"]
              , ssRedirectToIndex = False
              , ssAddTrailingSlash = True
              , ss404Handler = Just redirectApp
              , ssMaxAge = MaxAgeSeconds 3600
              }

-- | 404 handler.
-- We will redirect users to a 404 page if we can't locate the resource they
-- are looking for.
redirectApp :: Application
redirectApp req sendResponse = sendResponse $ redirectTo "/"

-- | Gzip compression middleware.
gzipMiddleware :: Middleware
gzipMiddleware = gzip def

-- | Domain redirection middleware.
-- When the site is live, we want to redirect users to the right domain name
-- regarles of whether they arrive from a www. domain, the server's IP address
-- or a spoof domain which is pointing to this server.
domainMiddleware :: Middleware
domainMiddleware = forceDomain
                    (\domain -> case domain of
                                 "localhost" -> Nothing
                                 "chromabits.com" -> Nothing
                                 _ -> Just "chromabits.com")

-- | Common headers middleware.
headersMiddleware :: Middleware
headersMiddleware = addHeaders
  [ ("Strict-Transport-Security", "max-age=31536000; includeSubdomains")
  , ("X-Frame-Options", "SAMEORIGIN")
  , ("X-XSS-Protection", "1; mode=block")
  , ("X-Content-Type-Options", "nosniff")
  ]

cspHeadersMiddleware :: Middleware
cspHeadersMiddleware = addHeaders
  [("Content-Security-Policy", TE.encodeUtf8 $ glue policies)]
  where
    glue :: [T.Text] -> T.Text
    glue [] = "default-src 'none'"
    glue [x] = x
    glue xs = T.concat $ map (\x -> T.concat [x, "; "]) (init xs) ++ [last xs]

    policies :: [T.Text]
    policies = [ "default-src 'self'"
               , "script-src 'self' 'unsafe-inline' https://use.typekit.net"
                  <> " https://cdn.mathjax.org https://connect.facebook.net"
                  <> " https://*.twitter.com https://cdn.syndication.twimg.com"
                  <> " https://gist.github.com"
                  <> " https://*.google-analytics.com/ga.js"
               , "img-src 'self' https: data: platform.twitter.com"
               , "font-src 'self' data: https://use.typekit.net"
                 <> " https://cdn.mathjax.org"
               , "style-src 'self' 'unsafe-inline' https://use.typekit.net"
                 <> " platform.twitter.com https://assets-cdn.github.com"
               , "frame-src https://www.youtube.com https://www.slideshare.net"
                 <> " staticxx.facebook.com www.facebook.com"
               ]

-- | De-indefify middleware.
-- Redirects any path ending in `/index.html` to just `/`.
deindexifyMiddleware :: Middleware
deindexifyMiddleware app req sendResponse =
  if lastMay (pathInfo req) == Just "index.html"
     then sendResponse $ redirectTo newPath
     else app req sendResponse
      where
        newPath :: BS.ByteString
        newPath = TE.encodeUtf8 $ processPath oldPath

        processPath :: [T.Text] -> T.Text
        processPath xs = case xs of
                           [] -> "/"
                           _ -> T.concat $ map prefixSlash xs

        oldPath :: [T.Text]
        oldPath = init $ pathInfo req

        prefixSlash :: T.Text -> T.Text
        prefixSlash x = T.concat ["/", x]

-- | Serves a WAI Application on the specified port.
-- The target port is printed to stdout before hand, which can be useful for
-- debugging purposes.
listen :: Int -> Application -> IO ()
listen port app = do
  -- Inform which port we will be listening on.
  putStrLn $ "Listening on port " ++ show port ++ "..."
  -- Serve the WAI app using Warp
  run port app

-- | Serves a WAI Application on the specified port.
-- The target port is printed to stdout before hand, which can be useful for
-- debugging purposes.
listenTLS :: Int -> Application -> IO ()
listenTLS port app = do
  certPath <- lookupEnv "BLOG_TLS_CERT"
  chainPath <- lookupEnv "BLOG_TLS_CHAIN"
  keyPath <- lookupEnv "BLOG_TLS_KEY"

  let tlsSettings = tlsSettingsChain
                      (fromMaybe "cert.pem" certPath)
                      [fromMaybe "fullchain.pem" chainPath]
                      (fromMaybe "privkey.pem" keyPath)
  let settings = setPort port defaultSettings

  -- Inform which port we will be listening on.
  putStrLn $ "Listening on port " ++ show port ++ " (TLS)..."
  -- Serve the WAI app using Warp
  runTLS tlsSettings settings app

-- | The entry point of the server application.
main :: IO ()
main = do
  stage <- lookupEnv "BLOG_STAGE"
  path <- lookupEnv "BLOG_PATH"

  let liveMiddleware = logStdout
                       $ cspHeadersMiddleware
                       $ headersMiddleware
                       $ domainMiddleware
                       $ deindexifyMiddleware
                       $ gzipMiddleware
                       $ staticSite path 

  -- Depending on the stage we will choose a different set of middleware to
  -- apply to the application.
  case fromMaybe "dev" stage of
    -- "Production"
    "live" -> do
      forkIO $ listenTLS 443 liveMiddleware
      listen 80 liveMiddleware
    "staging" -> do
      forkIO $ listenTLS 8443 liveMiddleware
      listen 8080 liveMiddleware
    -- "Development"
    _ -> listen 9090 (logStdout
                     $ headersMiddleware
                     $ deindexifyMiddleware
                     $ gzipMiddleware
                     $ staticSite path
                     )
