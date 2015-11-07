module Handler.Recipes where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              bfs)

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
    App {..} <- getYesod
    recipeSet <- liftIO (FDB.getRecipes appDatabase)
    recipeCategories <- liftIO (FDB.getCategories appDatabase)

    ((result, formWidget), formEnctype) <- runFormGet (sampleForm recipeCategories)

    let sortField = case result of
            FormSuccess (s,_) -> s
            _ -> Name

    let categoryFilter = case result of
            FormSuccess (_, Just c) -> (@= FDB.Category c)
            _ -> id

    let sorter = case sortField of
            Name -> IxSet.toAscList (IxSet.Proxy :: IxSet.Proxy FDB.Name)
            Category -> IxSet.toAscList (IxSet.Proxy :: IxSet.Proxy FDB.Category)
            Rating -> IxSet.toDescList (IxSet.Proxy :: IxSet.Proxy FDB.Rating)

    let recipes = sorter $ categoryFilter recipeSet

    defaultLayout $ do
        setTitle "Recipes"
        $(widgetFile "recipes")

dupe :: a -> (a, a)
dupe x = (x, x)

sampleForm :: [Text] -> Form (SortField, Maybe Text)
sampleForm categories = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> sortField
    <*> categoryFilterField
  where
    sortField = areq
      (selectField optionsEnum)
      (bfs ("Sort by: " :: Text))
      (Just Name)

    categoryFilterField = aopt
      (selectFieldList (map dupe categories))
      (bfs ("Category filter: " :: Text)) -- TODO: Add the 'mutiple' parameter with no value, might involve customising Yesod Forms
      Nothing

getSingleRecipeR :: Text -> Handler Html
getSingleRecipeR text = do
    App {..} <- getYesod
    recipeSet <- liftIO (FDB.getRecipes appDatabase)
    let maybeRecipe = IxSet.getOne (recipeSet @= FDB.Name text)

    defaultLayout $ do
        setTitle (toHtml text)
        $(widgetFile "single-recipe")
