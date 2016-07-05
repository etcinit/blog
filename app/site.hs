--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad (liftM)
import Control.Lens ((&), (.~))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (intersperse, isSuffixOf)
import Data.List.Split (splitOn)
import Hakyll
import Hakyll.Serve (ServeConfiguration, defaultServeConfiguration,
  hakyllConfiguration, hakyllServeWith)
import Text.Highlighting.Kate.Styles (haddock)
import Text.Pandoc.Options
import System.FilePath (splitExtension)
import System.Random (randomRIO)

--------------------------------------------------------------------------------
data SiteConfiguration = SiteConfiguration
  { siteRoot :: String
  , siteGaId :: String
  }

serveConf :: ServeConfiguration
serveConf = defaultServeConfiguration & hakyllConfiguration .~ hakyllConf

--------------------------------------------------------------------------------
hakyllConf :: Configuration
hakyllConf = defaultConfiguration
  { deployCommand =
      "rsync -ave 'ssh' _site/* 45.79.220.75:/var/www/chromabits " ++
      "&& rsync -ave 'ssh' " ++
      ".stack-work/install/x86_64-linux/lts-5.2/7.10.3/bin/server " ++
      "45.79.220.75:/opt/chromabits"
  }

siteConf :: SiteConfiguration
siteConf = SiteConfiguration
  { siteRoot = "https://chromabits.com"
  , siteGaId = "UA-47694260-1"
  }

-- feedConf :: String -> FeedConfiguration
-- feedConf title = FeedConfiguration
--  { feedTitle = "Chromabits: " ++ title
--  , feedDescription = "Personal blog"
--  , feedAuthorName = "Eduardo Trujillo"
--  , feedAuthorEmail = "ed@chromabits.com"
--  , feedRoot = "https://chromabits.com"
--  }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllServeWith serveConf $ do
  let writerOptions = defaultHakyllWriterOptions
        { writerHtml5 = True
        , writerHighlightStyle = haddock
        , writerHTMLMathMethod = MathJax $ "https://cdn.mathjax.org/"
            ++ "mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
        }

  let pandocHtml5Compiler =
        pandocCompilerWith defaultHakyllReaderOptions writerOptions

  match "images/*" $ do
    route idRoute
    compile copyFileCompiler
  
  match "images/posts/*" $ do
    route idRoute
    compile copyFileCompiler
  
  match "images/tumblr/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/app.css" $ do
    route $ setExtension "css"
    compile copyFileCompiler

  match "favicon.ico" $ do
    route $ setExtension "ico"
    compile copyFileCompiler

  match "keybase.txt" $ do
    route $ setExtension "txt"
    compile copyFileCompiler

  match "robots.txt" $ do
    route $ setExtension "txt"
    compile copyFileCompiler

  match "bower_components/font-awesome/fonts/*" $ do
    route $ gsubRoute "bower_components/font-awesome/" (const "")
    compile copyFileCompiler

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

  matchMetadata "posts/*" (M.member "legacy") $ version "legacy" $ do
    route $ legacyRoute `composeRoutes` setExtension "html"
    compile $ do
      color <- unsafeCompiler (randomRIO (0, length colors - 1)
        >>= \selection -> pure (colors !! selection))

      let ctx = constField "color" color `mappend`
            postCtx

      pandocHtml5Compiler
        >>= loadAndApplyTemplate "templates/post.html" ctx
        >>= loadAndApplyTemplate "templates/full-post.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
        >>= deIndexUrls

  match "posts/*" $ do
    route $ directorizeDate `composeRoutes` setExtension "html"
    compile $ do
      color <- unsafeCompiler (randomRIO (0, length colors - 1)
        >>= \selection -> pure (colors !! selection))

      let ctx = constField "color" color `mappend`
            postCtx

      pandocHtml5Compiler
        >>= loadAndApplyTemplate "templates/post.html" ctx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/full-post.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
        >>= deIndexUrls

  match "drafts/*" $ do
    route $ setExtension "html"
    compile $ do
      let ctx = constField "color" "red" `mappend` postCtx

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
      full <- loadAndApplyTemplate "templates/project.html"
        siteCtx compiled
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

      let archiveCtx =
            listField "posts" postCtx (return posts) `mappend`
            constField "title" "Archives" `mappend`
            siteCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls
        >>= deIndexUrls

  create ["projects.html"] $ do
    route indexify
    compile $ do
      projects <- loadAllSnapshots "projects/*" "teaser"

      let archiveCtx =
            listField "posts" siteCtx (return projects) `mappend`
            constField "title" "Projects" `mappend`
            siteCtx

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
      let ctx = paginateCtx `mappend` indexCtx

      loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
        >>= fmap (take 3) . recentFirst
        >>= applyTemplateList tpl ctx
        >>= makeItem
        >>= applyTemplate body (ctx `mappend` bodyField "posts")
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
        >>= deIndexUrls

    paginateRules pag $ \pageNum pattern -> do
      route idRoute
      compile $ do
        tpl <- loadBody "templates/post-item-full.html"

        let paginateCtx = paginateContext pag pageNum
        let ctx = paginateCtx `mappend`
              constField "title" ("Page " ++ show pageNum) `mappend`
              indexCtx

        loadAllSnapshots pattern "content"
          >>= recentFirst
          >>= applyTemplateList tpl ctx
          >>= makeItem
          >>= loadAndApplyTemplate "templates/paginated.html"
            (ctx `mappend` bodyField "posts")
          >>= loadAndApplyTemplate "templates/default.html"
            (ctx `mappend` bodyField "posts")
          >>= relativizeUrls
          >>= deIndexUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
siteCtx :: Context String
siteCtx =
  deIndexedUrlField "url" `mappend`
  constField "root" (siteRoot siteConf) `mappend`
  constField "gaId" (siteGaId siteConf) `mappend`
  defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    dateField "datetime" "%Y-%m-%d" `mappend`
    siteCtx

indexCtx :: Context String
indexCtx = siteCtx

--------------------------------------------------------------------------------
colors :: [String]
colors = ["purple", "yellow", "orange", "red", "cyan", "green", "blue"]

--------------------------------------------------------------------------------
directorizeDate :: Routes
directorizeDate = customRoute (\i -> directorize $ toFilePath i)
  where
    directorize path = dirs ++ "/index" ++ ext
      where
        (dirs, ext) = splitExtension $ concat $
          intersperse "/" date ++ ["/"] ++ intersperse "-" rest
        (date, rest) = splitAt 3 $ splitOn "-" path

indexify :: Routes
indexify = customRoute (\i -> addIndex $ toFilePath i)
  where
    addIndex path = original ++ "/index" ++ ext
     where
       (original, ext) = splitExtension path

grouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
grouper ids = (liftM (paginateEvery 3) . sortRecentFirst) ids

makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ "page/" ++ show pageNum ++ "/index.html"

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

-- | A special route that will produce paths compatible with the old Chromabits
-- blog. The slug in that path is determined by a 'legacy' field on each post.
legacyRoute :: Routes
legacyRoute = metadataRoute $
  \x -> constRoute $
    "post/" ++ fromMaybe "unknown" (M.lookup "legacy" x) ++ "/index.html"