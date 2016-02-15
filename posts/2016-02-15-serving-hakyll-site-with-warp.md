---
title: Serving a Hakyll site with Warp/Wai
date: Mon Feb 15 02:00:00 EDT 2016
author: Eduardo Trujillo
uuid: 16b3b144-0fb7-422c-a31e-0fca75e28206
---

Working with Hakyll has been great so far, but it seems the fun ends right
after your site is generated and copied over to the remote server. From there,
you have to deal with Nginx or Apache configuration files for the final
touches, such as redirections, cache policies, or additional headers.

I've seen applications, such as XMonad, which employ Haskell scripts in-lieu
of configuration files, and it's generally an enjoyable experience due to the
added flexibility and customization it provides.

With that in mind, wouldn't it be nice if the web server was in Haskell as
well? After all, Hakyll has a preview function that works well enough. So, it
shouldn't be too hard to replicate that and add some custom logic on top of it.

Well, that's exactly what I was wondering during the past few days, and
(perhaps not too) surprisingly, it only took half a day's worth of work to get
a working prototype.

To get the job done, I added a new target to my Hakyll project's cabal file,
and included `warp`, `wai-static-app`, and a bunch of WAI middleware
(`wai-extra`) as dependencies. The rest was merely a process of putting all
the pieces together and ensuring the site behaved as expected.

## The result

* A fast static site server.
* Support for HTTP 2.0 and HTTPS.
* Customized configurations for production, staging, and development.
* A more complete solution than `hakyll server`, since the site behaves
exactly like it would on production.
* A clear path for adding more logic in the future (API, Proxy, etc).

The server code is a few orders of magnitude larger than a simple configuration
file, but it's also under 200 lines of code.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent                   (forkIO)
import qualified Data.ByteString                      as BS (ByteString, pack)
import           Data.Maybe                           (fromMaybe, mapMaybe)
import           Data.Monoid                          ((<>))
import           Data.String                          (fromString)
import qualified Data.Text                            as T (Text, concat, pack)
import qualified Data.Text.Encoding                   as TE (encodeUtf8)
import           Network.Wai                          (Application, Middleware,
                                                       pathInfo)
import           Network.Wai.Application.Static       (defaultWebAppSettings,
                                                       ss404Handler,
                                                       ssAddTrailingSlash,
                                                       ssIndices, ssMaxAge,
                                                       ssRedirectToIndex,
                                                       staticApp)
import           Network.Wai.Handler.Warp             (defaultSettings, run,
                                                       setPort)
import           Network.Wai.Handler.WarpTLS          (runTLS,
                                                       tlsSettingsChain)
import           Network.Wai.Middleware.AddHeaders    (addHeaders)
import           Network.Wai.Middleware.ForceDomain   (forceDomain)
import           Network.Wai.Middleware.ForceSSL      (forceSSL)
import           Network.Wai.Middleware.Gzip          (def, gzip)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Network.Wai.Middleware.Vhost         (redirectTo)
import           Safe                                 (lastMay)
import           System.Environment                   (lookupEnv)
import           WaiAppStatic.Types                   (MaxAge (MaxAgeSeconds),
                                                       toPiece)

-- | The core application.
-- It serves files from `_site` which is where Hakyll will place the generated
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
  [ ("X-Frame-Options", "SAMEORIGIN")
  , ("X-XSS-Protection", "1; mode=block")
  , ("X-Content-Type-Options", "nosniff")
  ]

-- | Strict Transport Security middleware.
stsHeadersMiddleware :: Middleware
stsHeadersMiddleware = addHeaders
  [("Strict-Transport-Security", "max-age=31536000; includeSubdomains")]

-- | Content Security Policy middleware.
-- Here we add the CSP header which includes the policies for this blog.
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
                       $ forceSSL
                       $ deindexifyMiddleware
                       $ gzipMiddleware
                       $ staticSite path

  -- Depending on the stage we will choose a different set of middleware to
  -- apply to the application.
  case fromMaybe "dev" stage of
    -- "Production"
    "live" -> do
      forkIO $ listenTLS 443 $ stsHeadersMiddleware liveMiddleware
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

```

## Lets dive in!

Now let's dive into the details. The code above might not be too long, but its
doing a few interesting things that you might not usually get out of a simple 
static site server:

### forkIO

`forkIO` (from `Control.Concurrent`) was one of the most interesting things I
encountered while writing the server, and it also one of the last.

If you are writing a server to run on a single port, you don't generally stop
and ask yourself how you would modify it to run on multiple port
simultaneously.

Well, if you expect your site to use TLS and HTTP 2.0, you will most likely
want to also redirect users from the insecure site to the secure one. This
means you will need to listen on both, ports 80 and 443.

My initial approach to this was very naive. I concluded that I could simply
just run two servers by passing a flag at startup. However, this seemed
cumbersome and error-prone.

The real solution lead to learn a new part of Haskell that I had largely ignore
before: Concurrency.

I was please to find that `forkIO` allows you to create a lightweight thread
that can execute another `IO ()` function.

With it, I was able to spawn both servers at the same time:

```haskell
case fromMaybe "dev" stage of
-- "Production"
"live" -> do
  forkIO $ listenTLS 443 $ stsHeadersMiddleware liveMiddleware
  listen 80 liveMiddleware
```

### redirectTo

Hidden away on the `Network.Wai.Middleware.Vhost` module, you can find the
`redirectTo` function, which is really useful for sending redirect (301)
responses to clients.

On the server, I use it to define the 404 error handler:

```haskell
redirectApp :: Application
redirectApp req sendResponse = sendResponse $ redirectTo "/"
```

### forceDomain and forceSSL

To redirect users from an insecure (HTTP) site to a secure (HTTPS) site, the
you can use the `forceSSL`.

To redirect users to the right domain name, you can use `forceDomain`, however,
you need to pass in a custom function which specifies when and to which domains
users should be redirected to.

I prefer to use the naked domain for my blog, so I redirect anything that is
not `chromabits.com`:

```haskell
domainMiddleware :: Middleware
domainMiddleware = forceDomain
                    (\domain -> case domain of
                                 "localhost" -> Nothing
                                 "chromabits.com" -> Nothing
                                 _ -> Just "chromabits.com")
```

### No more index.html

One thing I don't like about static sites is having `.html` extensions on the
URL. In Hakyll, I managed to partially work around this problem by simply
making sure that most paths result in `index.html` (e.g. `/archive/` is
actually `/archive/index.html`). However, you could still technically get to
the `index.html` path if you visit it directly.

With this server, I wanted to add an additional redirect rule that would
redirect users away from paths with `index.html`. It is implemented on the
code above as `deindexifyMiddleware`.

In the future, I might consider writing a smarter middleware capable of
handling more paths (e.g. `/archive` would simply be `/archive.html`).

### Middleware stacks

Something that was not immediately obvious to me from reading the `wai`
documentation was how to chain middleware together. By looking at the types, I
eventually realized that a `Middleware` takes in an `Application` and returns a
new `Application` which is wrapped with the middleware. This means that you can
simply feed each middleware to the next one and end up with an application that
is wrapped in the entire stack.

```haskell
type Middleware = Application -> Application
```

However, you will need to pay special attention to the order in which they are
applied. For me, it was important that the logger middleware was at the top of
the stack so that every request was logged.

```haskell
-- Everything is logged.
site = logStdout $ forceSSL $ app

-- Some redirects will not be logged.
site = forceSSL $ logStdout $ app
```

## Overkill? Perhaps. Fun? Definitely.

Given that I'm trying to learn more about Haskell, I'm constantly trying to
find small projects to tackle with the language.

Writing your own server for a simple static site might sound overkill and,
most likely, take longer than dealing with a couple of Apache configuration
files.

That is, if your goal is to just get a site up, just stick to a pre-built
server, but if you are looking to learn, writing your own might be worth your
time.
