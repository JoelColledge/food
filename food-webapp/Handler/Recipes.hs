module Handler.Recipes where

import Import

-- import Data.Maybe(fromMaybe)

import qualified FoodDatabase as FDB
import Data.IxSet( (@=) )
import qualified Data.IxSet as IxSet(toList, getOne)
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
-- Handlers
--
getRecipesR :: Handler Html
getRecipesR = do
    App {..} <- getYesod
    recipeSet <- liftIO (FDB.getRecipes appDatabase)
    let recipes = IxSet.toList recipeSet

    defaultLayout $ do
        setTitle "Recipes"
        $(widgetFile "recipes")

getSingleRecipeR :: Text -> Handler Html
getSingleRecipeR text = do
    App {..} <- getYesod
    recipeSet <- liftIO (FDB.getRecipes appDatabase)
    let maybeRecipe = IxSet.getOne (recipeSet @= FDB.Name text)

    defaultLayout $ do
        setTitle (toHtml text)
        $(widgetFile "single-recipe")
