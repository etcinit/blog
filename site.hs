--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (liftM)
import Data.Monoid (mappend)
import Data.List (intersperse)
import Data.List.Split (splitOn)
import Hakyll
import System.FilePath (combine, splitExtension, takeFileName)
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/app.scss" $ do
    route $ setExtension "css"
    compile $ liftM (fmap compressCss) (getResourceString >>=
      withItemBody
        (unixFilter "sass" ["-s", "--scss", "--load-path=css"]))

  match (fromList ["about.md", "contact.markdown"]) $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "posts/*" $ do
    route $ directorizeDate `composeRoutes` setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"

      let archiveCtx =
            listField "posts" postCtx (return posts) `mappend`
            constField "title" "Archives" `mappend`
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  pag <- buildPaginateWith grouper "posts/*" makeId

  match "index.html" $ do
    route idRoute
    compile $ do
      tpl <- loadBody "templates/post-item-full.html"
      body <- readTemplate . itemBody <$> getResourceBody

      let paginateCtx = paginateContext pag 1
      let ctx = paginateCtx `mappend` indexCtx

      loadAllSnapshots "posts/*" "content"
        >>= fmap (take 100) . recentFirst
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
        let ctx = paginateCtx `mappend` indexCtx

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
indexCtx =
    constField "title" "Home" `mappend`
    defaultContext

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
makeId pageNum = fromFilePath $ "page/" ++ (show pageNum) ++ "/index.html"
