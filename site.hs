--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "js/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "index.html" $ do
    route idRoute
    compile $ do
      cards <- loadAll "cards/*"
      let indexCtx =
            listField "cards" defaultContext (pure cards) `mappend`
            defaultContext
      getResourceBody
        >>= loadAndApplyTemplate "templates/main.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

  match "cards/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/card.html" defaultContext
      >>= relativizeUrls


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext
