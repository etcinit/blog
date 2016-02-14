{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS (ByteString, pack)
import Data.String (fromString)
import qualified Data.Text as T (Text, concat, pack)
import qualified Data.Text.Encoding as TE (encodeUtf8)
import Data.Maybe (mapMaybe, fromMaybe)
import System.Environment (lookupEnv)
import Network.Wai (Application, Middleware, pathInfo)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Application.Static (staticApp
                                      , defaultWebAppSettings
                                      , ssIndices
                                      , ssRedirectToIndex
                                      , ssAddTrailingSlash
                                      , ss404Handler
                                      )
import WaiAppStatic.Types (toPiece)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Middleware.ForceSSL (forceSSL)
import Network.Wai.Middleware.ForceDomain (forceDomain)
import Network.Wai.Middleware.Vhost (redirectTo)

-- | The core application.
-- It serves files from `_site` whic is where Hakyll will place the generated
-- site.
staticSite :: Application
staticSite = staticApp
              (defaultWebAppSettings $ fromString "_site")
              { ssIndices  = mapMaybe (toPiece . T.pack) ["index.html"]
              , ssRedirectToIndex = False
              , ssAddTrailingSlash = True
              , ss404Handler = Just redirectApp
              }

-- | 404 handler.
-- We will redirect users back to the homepage if the reosurce they are looking
-- for is not found.
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

-- | De-indefify middleware.
-- Redirects any path ending in `/index.html` to just `/`.
deindexifyMiddleware :: Middleware
deindexifyMiddleware app req sendResponse =
  if maybeLast (pathInfo req) == Just "index.html"
     then sendResponse $ redirectTo newPath
     else app req sendResponse
      where
        newPath :: BS.ByteString
        newPath = TE.encodeUtf8 $ T.concat (map prefixSlash oldPath)

        oldPath :: [T.Text]
        oldPath = init $ pathInfo req

        prefixSlash :: T.Text -> T.Text
        prefixSlash x = T.concat ["/", x]

maybeLast :: [a] -> Maybe a
maybeLast xs = case xs of
                 [] -> Nothing
                 _ -> Just (last xs)

-- | Serves a WAI Application on the specified port.
-- The target port is printed to stdout before hand, which can be useful for
-- debugging purposes.
listen :: Int -> Application -> IO ()
listen port app = do
  -- Inform which port we will be listening on.
  putStrLn $ "Listening on port " ++ show port ++ "..."
  -- Serve the WAI app using Warp
  run port app

-- | The entry point of the server application.
main :: IO ()
main = do
  stage <- lookupEnv "BLOG_STAGE"

  -- Depending on the stage we will choose a different set of middleware to
  -- apply to the application.
  case fromMaybe "dev" stage of
    -- "Production"
    "live" -> listen 80 (logStdout 
                        $ domainMiddleware
                        $ deindexifyMiddleware
                        $ gzipMiddleware staticSite
                        )
    -- "Development"
    _ -> listen 9090 (logStdout 
                     $ deindexifyMiddleware 
                     $ gzipMiddleware staticSite
                     )
