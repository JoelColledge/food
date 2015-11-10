module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              bfs)

import qualified FoodDatabase as FDB

getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let imported = False

    let debugText = "" :: String

    defaultLayout $ do
        setTitle "Colledge Food"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm

    App {..} <- getYesod
    liftIO (FDB.importFullRecipes appDatabase "Sources.csv" "Categories.csv" "Recipes.csv")

    let imported = True

    let debugText = show result

    defaultLayout $ do
        setTitle "Colledge Food"
        $(widgetFile "homepage")

-- -- | A generalized version of 'parseHelper', allowing any type for the message
-- -- indicating a bad parse.
-- --
-- -- Since 1.3.6
multiParseHelper :: (Monad m, RenderMessage site FormMessage)
               => ([Text] -> Either FormMessage a)
               -> [Text] -> [FileInfo] -> m (Either (SomeMessage site) (Maybe a))
multiParseHelper _ [] _ = return $ Right Nothing
multiParseHelper f xs _ = return $ either (Left . SomeMessage) (Right . Just) $ f xs

parseVals :: [Text] -> Either FormMessage [Text]
parseVals [] = Right []
parseVals xs = Right xs

-- | Creates a input with @type="text"@.
multiTextField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m [Text]
multiTextField = Field
    { fieldParse = multiParseHelper $ parseVals
    , fieldView = \theId name attrs val isReq ->
        [whamlet|
<input id="#{theId}" name="#{name}" *{attrs} type="text" :isReq:required value="">
<input id="another_id" name="#{name}" *{attrs} type="text" :isReq:required value="">
|]
    , fieldEnctype = UrlEncoded
    }

sampleForm :: Form ([Text])
sampleForm = renderBootstrap3 BootstrapBasicForm $
    areq
      multiTextField
      (bfs ("Name" :: Text))
      Nothing
