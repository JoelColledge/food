module Handler.Recipes where

import Import

-- import Data.Maybe(fromMaybe)

import qualified FoodDatabase as FDB
import Data.IxSet( (@=) )
import qualified Data.IxSet as IxSet(toList, getOne)
-- import qualified Data.Map.Strict as Map(findWithDefault, lookup)

--
-- Reprocessing database content into an easier to use type
--
data AdornedRecipe = AdornedRecipe {
    adorned_recipe :: FDB.Recipe,
    adorned_full_book :: (Maybe Text, Maybe Int),
    adorned_full_category :: Maybe Text
  } deriving (Show)

maybeBookCode :: FDB.Recipe -> Maybe (Text, Maybe Int)
maybeBookCode recipe = case FDB.recipe_source recipe of
    FDB.RecipeBook book_code maybePage -> Just (book_code, maybePage)
    _ -> Nothing

toAdorned :: FDB.BookMap -> FDB.CategoryMap -> FDB.Recipe -> AdornedRecipe
toAdorned bookMap categoryMap recipe =
    AdornedRecipe recipe full_book (lookup category categoryMap)
  where full_book :: (Maybe Text, Maybe Int)
        full_book = case maybeBookCode recipe of
          Just (book_code, maybePage) -> (book_code `lookup` bookMap, maybePage)
          Nothing -> (Nothing, Nothing)
        category = FDB.getCategory (FDB.recipe_category recipe)

getAdName :: AdornedRecipe -> Text
getAdName = FDB.getName . FDB.recipe_name . adorned_recipe

getAdCategory :: AdornedRecipe -> Text
getAdCategory AdornedRecipe{..} =
    fromMaybe category adorned_full_category
  where category = FDB.getCategory (FDB.recipe_category adorned_recipe)

getAdMaybeBook :: AdornedRecipe -> Maybe Text
getAdMaybeBook AdornedRecipe{..} = do
    (book_code, _) <- maybeBookCode adorned_recipe
    return (fromMaybe book_code (fst adorned_full_book))

getAdComments :: AdornedRecipe -> Text
getAdComments AdornedRecipe{..} = FDB.recipe_comments adorned_recipe

--
-- Handlers
--
getRecipesR :: Handler Html
getRecipesR = do
    App {..} <- getYesod
    recipeSet <- liftIO (FDB.getRecipes appDatabase)
    bookMap <- liftIO (FDB.getBooks appDatabase)
    categoryMap <- liftIO (FDB.getCategories appDatabase)
    let adRecipes = map (toAdorned bookMap categoryMap) $ IxSet.toList recipeSet

    defaultLayout $ do
        setTitle "Recipes"
        $(widgetFile "recipes")

getSingleRecipeR :: Text -> Handler Html
getSingleRecipeR text = do
    App {..} <- getYesod
    recipeSet <- liftIO (FDB.getRecipes appDatabase)
    bookMap <- liftIO (FDB.getBooks appDatabase)
    categoryMap <- liftIO (FDB.getCategories appDatabase)

    let maybeRecipe = IxSet.getOne (recipeSet @= FDB.Name text)
    let maybeAdRecipe = fmap (toAdorned bookMap categoryMap) maybeRecipe

    defaultLayout $ do
        setTitle (toHtml text)
        $(widgetFile "single-recipe")
