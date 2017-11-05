{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad       (liftM, mapM_, join)
import Data.List           (intersperse, isSuffixOf)
import Data.List.Split     (splitOn)
import Data.Maybe          (fromMaybe)
import Data.Monoid         ((<>))
import System.FilePath     (splitExtension)
import System.Random       (randomRIO)

import           Control.Lens                  ((&), (.~))
import qualified Data.Aeson                    as A
import           Data.Default                  (def)
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import           Hakyll
import           Hakyll.Serve.Main             (HakyllServeConfiguration,
                                                hakyllServeWith,
                                                hscHakyllConfiguration)
import           Skylighting.Styles (haddock)
import           Text.Pandoc.Options
import Text.Sass.Compilation
import Text.Sass.Options

-- TYPES ----------------------------------------------------------------------

data SiteConfiguration = SiteConfiguration
  { siteRoot :: String
  , siteGaId :: String
  }

-- CONFIGURATION --------------------------------------------------------------

serveConf :: HakyllServeConfiguration
serveConf = def & hscHakyllConfiguration .~ hakyllConf

hakyllConf :: Configuration
hakyllConf = defaultConfiguration
  { deployCommand
      = "rsync -ave 'ssh' _site/* 45.79.220.75:/var/www/chromabits "
      <> "&& rsync -ave 'ssh' "
      <> ".stack-work/install/x86_64-linux/lts-6.7/7.10.3/bin/server "
      <> "45.79.220.75:/opt/chromabits"
  }

siteConf :: SiteConfiguration
siteConf = SiteConfiguration
  { siteRoot = "https://chromabits.com"
  , siteGaId = "UA-47694260-1"
  }

feedConf :: FeedConfiguration
feedConf = FeedConfiguration
  { feedTitle = "Chromabits"
  , feedDescription = "A personal blog"
  , feedAuthorName = "Eduardo Trujillo"
  , feedAuthorEmail = "ed+contact@chromabits.com"
  , feedRoot = "https://chromabits.com"
  }

colors :: [String]
colors = ["purple", "yellow", "orange", "red", "cyan", "green", "blue"]

-- RULES ----------------------------------------------------------------------

main :: IO ()
main = hakyllServeWith serveConf $ do
  let writerOptions = defaultHakyllWriterOptions
        { writerHtml5 = True
        , writerHighlightStyle = haddock
        , writerHTMLMathMethod = MathJax
            $ "https://cdn.mathjax.org/"
            <> "mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
        }
      pandocHtml5Compiler = pandocCompilerWith
        defaultHakyllReaderOptions
        writerOptions

  mapM_ matchAndCopyDirectory
    ["images/*", "images/posts/*", "images/tumblr/*"]
  mapM_ matchAndCopy
    [ ("favicon.ico", "ico")
    , ("keybase.txt", "txt")
    , ("robots.txt", "txt")
    ]
  match "third_party/font-awesome/fonts/*" $ do
    route $ gsubRoute "third_party/font-awesome/" (const "")
    compile copyFileCompiler

  create ["scss/app.scss"] $ do
    route $ gsubRoute "scss/" (const "css/") `composeRoutes` setExtension "css"
    compile . sassCompiler $ def
      { sassIncludePaths = Just 
          [ "third_party/foundation-sites/scss"
          , "third_party/motion-ui/src"
          , "third_party/font-awesome/scss"
          ]
      }

  create ["404.html"] $ do
    route $ setExtension "html"
    compile $ pandocHtml5Compiler
      >>= loadAndApplyTemplate "templates/default.html" siteCtx
      >>= relativizeUrls
      >>= deIndexUrls

  create ["about.html"] $ do
    route $ indexify `composeRoutes` setExtension "html"
    compile $ pandocHtml5Compiler
      >>= loadAndApplyTemplate "templates/default.html" siteCtx
      >>= relativizeUrls
      >>= deIndexUrls

  matchMetadata "posts/*" (HM.member "legacy") $ version "legacy" $ do
    route $ legacyRoute `composeRoutes` setExtension "html"
    compile $ do
      color <- unsafeCompiler pickColor
      identifier <- getUnderlying

      let ctx
            = constField "color" color
            <> constField "identifier" (show identifier)
            <> postCtx

      pandocHtml5Compiler
        >>= loadAndApplyTemplate "templates/post.html" ctx
        >>= loadAndApplyTemplate "templates/full-post.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
        >>= deIndexUrls

  match "posts/*" . version "markdown" $ do
    route idRoute
    compile copyFileCompiler

  match "posts/*" $ do
    route $ directorizeDate "/index" `composeRoutes` setExtension "html"
    compile $ do
      color <- unsafeCompiler pickColor
      identifier <- getUnderlying

      let ctx
            = constField "color" color
            <> constField "identifier" (show identifier)
            <> postCtx

      pandocHtml5Compiler
        >>= loadAndApplyTemplate "templates/post-body.html" ctx
        >>= saveSnapshot "content-body"
        >>= loadAndApplyTemplate "templates/post.html" ctx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/full-post.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
        >>= deIndexUrls

  match "drafts/*" $ do
    route $ setExtension "html"
    compile $ do
      let ctx = constField "color" "red" <> postCtx

      pandocHtml5Compiler
        >>= loadAndApplyTemplate "templates/post.html" ctx
        >>= loadAndApplyTemplate "templates/full-post.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
        >>= deIndexUrls

  match "projects/*" $ do
    route $ indexify `composeRoutes` setExtension "html"
    compile $ do
      compiled <- pandocHtml5Compiler
      full <- loadAndApplyTemplate
        "templates/project.html"
        siteCtx
        compiled
      teaser <- loadAndApplyTemplate "templates/project-teaser.html"
        siteCtx $ dropMore compiled

      _ <- saveSnapshot "teaser" teaser

      saveSnapshot "content" full
        >>= loadAndApplyTemplate "templates/default.html" siteCtx
        >>= relativizeUrls
        >>= deIndexUrls

  create ["archive.html"] $ do
    route indexify
    compile $ do
      posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)

      let archiveCtx
            = listField "posts" postCtx (return posts)
            <> constField "title" "Archives"
            <> siteCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls
        >>= deIndexUrls

  create ["projects.html"] $ do
    route indexify
    compile $ do
      projects <- loadAllSnapshots "projects/*" "teaser"

      let archiveCtx
            = listField "posts" siteCtx (return projects)
            <> constField "title" "Projects"
            <> siteCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/projects.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls
        >>= deIndexUrls

  pag <- buildPaginateWith grouper ("posts/*" .&&. hasNoVersion) makeId

  match "index.html" $ do
    route idRoute
    compile $ do
      tpl <- loadBody "templates/post-item-full.html"
      body <- readTemplate . itemBody <$> getResourceBody

      let paginateCtx = paginateContext pag 1
      let ctx = paginateCtx <> indexCtx

      loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
        >>= fmap (take 3) . recentFirst
        >>= applyTemplateList tpl ctx
        >>= makeItem
        >>= applyTemplate body (ctx <> bodyField "posts")
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
        >>= deIndexUrls

  paginateRules pag $ \pageNum patterns -> do
    route idRoute
    compile $ do
      template <- loadBody "templates/post-item-full.html"

      let context
            = paginateContext pag pageNum
            <> constField "title" ("Page " <> show pageNum)
            <> indexCtx
          bodyContext = context <> bodyField "posts"

      loadAllSnapshots patterns "content"
        >>= recentFirst
        >>= applyTemplateList template context
        >>= makeItem
        >>= loadAndApplyTemplate "templates/paginated.html" bodyContext
        >>= loadAndApplyTemplate "templates/default.html" bodyContext
        >>= relativizeUrls
        >>= deIndexUrls

  match "templates/*" $ compile templateCompiler

  create ["feed.rss"] $ do
    route idRoute
    compile $ do
      let context = postCtx <> bodyField "description"

      posts <- fmap (take 10) . recentFirst
        =<< loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content-body"
      renderRss feedConf context posts

-- CONTEXTS -------------------------------------------------------------------

siteCtx :: Context String
siteCtx
  = deIndexedUrlField "url"
  <> constField "root" (siteRoot siteConf)
  <> constField "gaId" (siteGaId siteConf)
  <> defaultContext

postCtx :: Context String
postCtx
  = dateField "date" "%B %e, %Y"
  <> dateField "datetime" "%Y-%m-%d"
  <> siteCtx

indexCtx :: Context String
indexCtx = siteCtx

-- ROUTE HELPERS --------------------------------------------------------------

directorizeDate :: String -> Routes
directorizeDate postfix = customRoute (directorize . toFilePath)
  where
    directorize path = dirs <> postfix <> ext
     where
      (dirs, ext) = splitExtension . concat
        $ intersperse "/" date ++ ["/"] ++ intersperse "-" rest
      (date, rest) = splitAt 3 $ splitOn "-" path

indexify :: Routes
indexify = customRoute (addIndex . toFilePath)
  where
    addIndex path = original ++ "/index" ++ ext
     where
       (original, ext) = splitExtension path

-- | A special route that will produce paths compatible with the old Chromabits
-- blog. The slug in that path is determined by a 'legacy' field on each post.
legacyRoute :: Routes
legacyRoute = metadataRoute $ \x -> constRoute . T.unpack . mconcat $
  [ "post/"
  , fromMaybe "unknown" (HM.lookup "legacy" x >>= valueToText)
  , "/index.html"
  ]

-- RULE HELPERS ---------------------------------------------------------------

matchAndCopyDirectory :: Pattern -> Rules ()
matchAndCopyDirectory dir = match dir $ do
  route idRoute
  compile copyFileCompiler

matchAndCopy :: (Pattern, String) -> Rules ()
matchAndCopy (path, extension) = match path $ do
  route $ setExtension extension
  compile copyFileCompiler

-- IDENTIFIER HELPERS ---------------------------------------------------------

grouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
grouper = fmap (paginateEvery 3) . sortRecentFirst

makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ "page/" ++ show pageNum ++ "/index.html"

-- SASS COMPILER --------------------------------------------------------------

sassCompiler :: SassOptions -> Compiler (Item String)
sassCompiler options = getResourceBody >>= compileSass options
  where
    compileSass :: SassOptions -> Item String -> Compiler (Item String)
    compileSass options item = join $ unsafeCompiler $ do
      result <- compileFile (toFilePath $ itemIdentifier item) options
      case result of
        Left sassError -> errorMessage sassError >>= fail
        Right result_ -> pure $ makeItem result_

-- UTILITIES ------------------------------------------------------------------

pickColor :: IO String
pickColor = do
  selection <- randomRIO (0, length colors - 1)
  pure $ colors !! selection

stripIndex :: String -> String
stripIndex url = if "index.html" `isSuffixOf` url
    && elem (head url) ("/." :: String)
  then take (length url - 10) url else url

deIndexUrls :: Item String -> Compiler (Item String)
deIndexUrls item = return $ fmap (withUrls stripIndex) item

deIndexedUrlField :: String -> Context a
deIndexedUrlField key = field key
  $ fmap (stripIndex . maybe empty toUrl) . getRoute . itemIdentifier

dropMore :: Item String -> Item String
dropMore = fmap (unlines . takeWhile (/= "<!--more-->") . lines)

valueToText :: A.Value -> Maybe T.Text
valueToText (A.String innerText) = Just innerText
valueToText _ = Nothing
