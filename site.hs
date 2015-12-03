--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (liftM)
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import Data.List (intersperse)
import Data.List.Split (splitOn)
import Hakyll
import Text.Highlighting.Kate.Styles (haddock)
import Text.Pandoc.Options
import System.FilePath (combine, splitExtension, takeFileName)
import System.Random (randomRIO)

--------------------------------------------------------------------------------
data SiteConfiguration = SiteConfiguration
  { siteRoot :: String
  , siteGaId :: String
  }

--------------------------------------------------------------------------------
hakyllConf :: Configuration
hakyllConf = defaultConfiguration
  { deployCommand = "rsync -ave 'ssh' _site/* chromabits.com:www/chromabits"
  }

siteConf :: SiteConfiguration
siteConf = SiteConfiguration
  { siteRoot = "https://chromabits.com"
  , siteGaId = "UA-47694260-1"
  }

feedConf :: String -> FeedConfiguration
feedConf title = FeedConfiguration
  { feedTitle = "Chromabits: " ++ title
  , feedDescription = "Personal blog"
  , feedAuthorName = "Eduardo Trujillo"
  , feedAuthorEmail = "ed@chromabits.com"
  , feedRoot = "https://chromabits.com"
  }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith hakyllConf $ do
  let writerOptions = defaultHakyllWriterOptions
        { writerHtml5 = True
        , writerHighlightStyle = haddock
        }

  let pandocHtml5Compiler =
        pandocCompilerWith defaultHakyllReaderOptions writerOptions

  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/app.scss" $ do
    route $ setExtension "css"
    compile $ liftM (fmap compressCss) (getResourceString >>=
      withItemBody
        (unixFilter "sass" ["-s", "--scss", "--load-path=css"]))

  match (fromList ["about.md", "projects.md"]) $ do
    route $ setExtension "html"
    compile $ pandocHtml5Compiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

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

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)

      let archiveCtx =
            listField "posts" postCtx (return posts) `mappend`
            constField "title" "Archives" `mappend`
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

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
          >>= loadAndApplyTemplate "templates/paginated.html" (ctx `mappend` bodyField "posts")
          >>= loadAndApplyTemplate "templates/default.html" (ctx `mappend` bodyField "posts")
          >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    dateField "datetime" "%Y-%m-%d" `mappend`
    defaultContext

indexCtx :: Context String
indexCtx = defaultContext

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

grouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
grouper ids = (liftM (paginateEvery 3) . sortRecentFirst) ids

makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ "page/" ++ show pageNum ++ "/index.html"

-- | A special route that will produce paths compatible with the old Chromabits
-- blog. The slug in that path is determined by a 'legacy' field on each post.
legacyRoute :: Routes
legacyRoute = metadataRoute $
  \x -> constRoute $
    "post/" ++ fromMaybe "unknown" (M.lookup "legacy" x) ++ "/index.html"
