{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe         (fromMaybe)
import Data.Monoid        ((<>))
import System.Environment (lookupEnv)

import Control.Lens
import Data.Default                 (def)
import Network.Wai.Serve.Listeners  (TLSSettings, tlsSettingsChain)
import Network.Wai.Serve.Main       (serve)
import Network.Wai.Serve.Middleware (cspHeadersMiddleware,
                                     deindexifyMiddleware, domainMiddleware,
                                     forceSSLMiddleware, gzipMiddleware,
                                     loggerMiddleware,
                                     securityHeadersMiddleware,
                                     stsHeadersMiddleware, (<#>))
import Network.Wai.Serve.Types      (Directive (..), Stage (..),
                                     TLSConfiguration (..), scDevTransform,
                                     scMiddleware, scPath, scPort,
                                     scProdTransform, scStage,
                                     scStagingTransform, scTlsConfiguration,
                                     tlsPort, tlsSettings)

directives :: [Directive]
directives =
  [ DefaultSrc ["'self'"]
  , ScriptSrc [
      "'self'", "'unsafe-inline'", "https://use.typekit.net",
      "https://cdn.mathkax.org", "https://connect.facebook.net",
      "https://*.twitter.com", "https://cdn.syndication.twimg.com",
      "https://gist.github.com"
    ]
  , ImgSrc ["'self'", "https:", "data:", "platform.twitter.com"]
  , FontSrc [
      "'self'", "data:", "https://use.typekit.net",
      "https://cdn.mathjax.org", "https://fonts.typekit.net"
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
        <#> domainMiddleware "chromabits.com"
        <#> forceSSLMiddleware
        <#> deindexifyMiddleware
        <#> gzipMiddleware
      prodMiddlware = (mempty <#> stsHeadersMiddleware) <> liveMiddleware

  let tlsConf = TLSConfiguration (const liveMiddleware) tlsSettings 8443

  serve $ def
    & scStage .~ case rawStage of
      Just "live" -> Production
      Just "staging" -> Staging
      _ -> Development
    & scPort .~ 9090
    & scMiddleware .~ mempty
      <#> loggerMiddleware
      <#> securityHeadersMiddleware
      <#> deindexifyMiddleware
      <#> gzipMiddleware
    & scPath .~ rawPath
    & scStagingTransform .~
      ( (set scTlsConfiguration $ Just tlsConf)
      . (set scMiddleware liveMiddleware)
      . (set scPort 8080)
      )
    & scProdTransform .~
      ( (set scTlsConfiguration $ Just (tlsConf & tlsPort .~ 443))
      . (set scMiddleware prodMiddlware)
      . (set scPort 80)
      )
