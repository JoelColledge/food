module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

import qualified FoodDatabase as FDB

getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let imported = False

    defaultLayout $ do
        setTitle "Colledge Food"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((_, formWidget), formEnctype) <- runFormPost sampleForm

    App {..} <- getYesod
    liftIO (FDB.importCsvRecipes appDatabase "Recipes.csv")
    liftIO (FDB.importCsvBooks appDatabase "Sources.csv")
    liftIO (FDB.importCsvCategories appDatabase "Categories.csv")

    let imported = True

    defaultLayout $ do
        setTitle "Colledge Food"
        $(widgetFile "homepage")

sampleForm :: Form ()
sampleForm = renderBootstrap3 BootstrapBasicForm $ mempty
