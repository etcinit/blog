{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Hakyll.Serve.Main (TLSConfiguration(..), Stage(..),
  defaultServeConfiguration, port, stage, middleware, stagingTransform,
  tlsConfiguration, tlsPort, prodTransform, path, serve)
import Hakyll.Serve.Middleware (Directive(..), (<#>), gzipMiddleware,
  domainMiddleware, securityHeadersMiddleware, stsHeadersMiddleware,
  cspHeadersMiddleware, deindexifyMiddleware, forceSSLMiddleware,
  loggerMiddleware)
import Hakyll.Serve.Listeners (TLSSettings, tlsSettingsChain)

directives :: [Directive]
directives
  = [ DefaultSrc ["'self'"]
    , ScriptSrc [
        "'self'", "'unsafe-inline'", "https://use.typekit.net",
        "https://cdn.mathkax.org", "https://connect.facebook.net",
        "https://*.twitter.com", "https://cdn.syndication.twimg.com",
        "https://gist.github.com"
      ]
    , ImgSrc ["'self'", "https:", "data:", "platform.twitter.com"]
    , FontSrc [
        "'self'", "data:", "https://use.typekit.net", "https://cdn.mathjax.org"
      ]
    , StyleSrc [
        "'self'", "'unsafe-inline'", "https://use.typekit.net",
        "platform.twitter.com", "https://assets-cdn.github.com"
      ]
    , FrameSrc [
        "https://www.youtube.com", "https://www.slideshare.net",
        "staticxx.facebook.com", "www.facebook.com"
      ]
    ]

getTLSSettings :: IO TLSSettings
getTLSSettings = do
  certPath <- lookupEnv "BLOG_TLS_CERT"
  chainPath <- lookupEnv "BLOG_TLS_CHAIN"
  keyPath <- lookupEnv "BLOG_TLS_KEY"

  return $ tlsSettingsChain
            (fromMaybe "cert.pem" certPath)
            [fromMaybe "fullchain.pem" chainPath]
            (fromMaybe "privkey.pem" keyPath)

-- | The entry point of the server application.
main :: IO ()
main = do
  rawStage <- lookupEnv "BLOG_STAGE"
  rawPath <- lookupEnv "BLOG_PATH"

  tlsSettings <- getTLSSettings 

  let liveMiddleware
        = mempty
        <#> loggerMiddleware
        <#> cspHeadersMiddleware directives
        <#> securityHeadersMiddleware
        <#> domainMiddleware "chromabits"
        <#> forceSSLMiddleware
        <#> deindexifyMiddleware
        <#> gzipMiddleware
  let prodMiddlware = (mempty <#> stsHeadersMiddleware) <> liveMiddleware

  let tlsConf = TLSConfiguration (const liveMiddleware) tlsSettings 8443

  let serveConf
        = defaultServeConfiguration
        & stage .~ case rawStage of
          Just "live" -> Production
          Just "staging" -> Staging
          _ -> Development
        & port .~ 9090
        & middleware .~ mempty
          <#> loggerMiddleware
          <#> securityHeadersMiddleware
          <#> deindexifyMiddleware
          <#> gzipMiddleware
        & path .~ rawPath
        & stagingTransform .~
          ((set tlsConfiguration $ Just tlsConf)
          . (set middleware liveMiddleware)
          . (set port 8080))
        & prodTransform .~
          ((set tlsConfiguration $ Just (tlsConf & tlsPort .~ 443))
          . (set middleware prodMiddlware)
          . (set port 80))

  serve serveConf
