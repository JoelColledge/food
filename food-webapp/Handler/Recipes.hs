module Handler.Recipes where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

-- import Data.Maybe(fromMaybe)

import qualified FoodDatabase as FDB
import Data.IxSet( (@=) )
import qualified Data.IxSet as IxSet(toAscList, toDescList, toList, getOne, Proxy(..))
-- import qualified Data.Map.Strict as Map(findWithDefault, lookup)

--
-- Reprocessing database content
--
getName :: FDB.Recipe -> Text
getName = FDB.getName . FDB.recipe_name

getCategory :: FDB.Recipe -> Text
getCategory = FDB.getCategory . FDB.recipe_category

getMaybeBook :: FDB.Recipe -> Maybe Text
getMaybeBook recipe = case FDB.recipe_source recipe of
    FDB.RecipeBook book _ -> Just book
    _ -> Nothing

getMaybePage :: FDB.Recipe -> Maybe Int
getMaybePage recipe = case FDB.recipe_source recipe of
    FDB.RecipeBook _ maybePage -> maybePage
    _ -> Nothing

getMaybeRating :: FDB.Recipe -> Maybe Int
getMaybeRating = FDB.ratingToMaybe . FDB.recipe_rating

getComments :: FDB.Recipe -> Text
getComments = FDB.recipe_comments

--
-- Filtering and sorting
--
data SortField = Name | Category | Rating
  deriving (Eq, Ord, Enum, Bounded, Show, Read)



--
-- Handlers
--
getRecipesR :: Handler Html
getRecipesR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
--     let submission = Nothing :: Maybe (SortField, Text)

    App {..} <- getYesod
    recipeSet <- liftIO (FDB.getRecipes appDatabase)
    let recipes = IxSet.toList recipeSet

    defaultLayout $ do
        setTitle "Recipes"
        $(widgetFile "recipes")

postRecipesR :: Handler Html
postRecipesR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let sortField = case result of
            FormSuccess s -> s
            _ -> Name

    let sorter = case sortField of
            Name -> IxSet.toAscList (IxSet.Proxy :: IxSet.Proxy FDB.Name)
            Category -> IxSet.toAscList (IxSet.Proxy :: IxSet.Proxy FDB.Category)
            Rating -> IxSet.toDescList (IxSet.Proxy :: IxSet.Proxy FDB.Rating)

    App {..} <- getYesod
    recipeSet <- liftIO (FDB.getRecipes appDatabase)
    let recipes = sorter recipeSet

    defaultLayout $ do
        setTitle "Recipes"
        $(widgetFile "recipes")

sampleForm :: Form (SortField)
sampleForm = renderBootstrap3 BootstrapBasicForm $ {-(,)-}
    {-<$> -}areq (selectField optionsEnum) "Sort by" (Just Name)
--     <*> areq textField (withSmallInput "Message") Nothing


getSingleRecipeR :: Text -> Handler Html
getSingleRecipeR text = do
    App {..} <- getYesod
    recipeSet <- liftIO (FDB.getRecipes appDatabase)
    let maybeRecipe = IxSet.getOne (recipeSet @= FDB.Name text)

    defaultLayout $ do
        setTitle (toHtml text)
        $(widgetFile "single-recipe")
