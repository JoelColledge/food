module Handler.Recipes where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              bfs)

import Handler.MultiField

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

getIsFolder :: FDB.Recipe -> Bool
getIsFolder recipe = case FDB.recipe_source recipe of
    FDB.RecipeFolder -> True
    _ -> False

getMaybeBook :: FDB.Recipe -> Maybe Text
getMaybeBook recipe = case FDB.recipe_source recipe of
    FDB.RecipeBook book _ -> Just book
    _ -> Nothing

getMaybePage :: FDB.Recipe -> Maybe Int
getMaybePage recipe = case FDB.recipe_source recipe of
    FDB.RecipeBook _ maybePage -> maybePage
    _ -> Nothing

getMaybeUrl :: FDB.Recipe -> Maybe Text
getMaybeUrl recipe = case FDB.recipe_source recipe of
    FDB.RecipeOnline url -> Just url
    _ -> Nothing

getMaybeRating :: FDB.Recipe -> Maybe Int
getMaybeRating = FDB.ratingToMaybe . FDB.recipe_rating

getComments :: FDB.Recipe -> Text
getComments = FDB.recipe_comments

getIngredients :: FDB.Recipe -> [(Text, Maybe Double, Text)]
getIngredients = FDB.getIngredients . FDB.recipe_ingredients

constructSource :: Bool -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe FDB.RecipeSource
constructSource isFolder maybeBook maybePage maybeUrl = constructSource' sources
  where
    sources = catMaybes [
        if isFolder then Just FDB.RecipeFolder else Nothing,
        fmap (flip FDB.RecipeBook maybePage) maybeBook,
        fmap FDB.RecipeOnline maybeUrl
      ]
    constructSource' [] = Just FDB.RecipeUnknown
    constructSource' [source] = Just source
    constructSource' _ = Nothing

constructRating :: Maybe Int -> FDB.Rating
constructRating Nothing = FDB.RatingNone
constructRating (Just rating) = FDB.Rating rating

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

    ((result, formWidget), formEnctype) <- runFormGet (filterForm recipeCategories)

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

filterForm :: [Text] -> Form (SortField, Maybe Text)
filterForm categories = renderBootstrap3 BootstrapBasicForm $ (,)
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

data RecipeFormType = NewRecipe
                    | UpdateRecipe Text
  deriving (Eq, Show, Read)

getSingleRecipeR :: Text -> Handler Html
getSingleRecipeR text = do
    App {..} <- getYesod
    recipeSet <- liftIO (FDB.getRecipes appDatabase)
    let maybeRecipe = IxSet.getOne (recipeSet @= FDB.Name text)
    let baseRecipe = fromMaybe (FDB.emptyRecipe { FDB.recipe_name = FDB.Name text }) maybeRecipe

    getRecipeForm (UpdateRecipe text) text (Just baseRecipe)

postSingleRecipeR :: Text -> Handler Html
postSingleRecipeR text = do
    postRecipeForm (UpdateRecipe text) text (Just text)

getAddRecipeR :: Handler Html
getAddRecipeR = do
    getRecipeForm NewRecipe "Add recipe" Nothing

postAddRecipeR :: Handler Html
postAddRecipeR = do
    postRecipeForm NewRecipe "Add recipe" Nothing

getRecipeForm :: RecipeFormType -> Text -> Maybe FDB.Recipe -> Handler Html
getRecipeForm formType title maybeRecipe = do
    App {..} <- getYesod
    recipeCategories <- liftIO (FDB.getCategories appDatabase)

    (formWidget, formEnctype) <- generateFormPost (recipeForm recipeCategories maybeRecipe)

    defaultLayout $ do
        let errorMsgs = [] :: [Text]
        setTitle (toHtml title)
        $(widgetFile "recipe-editor")


type RecipeFormReturn = (Text, Text,
  Bool, Maybe Text, Maybe Int, Maybe Text,
  Maybe Int, Maybe Textarea, Maybe [(Text, Maybe Double, Text)])

maybeLeft :: Either a b -> Maybe a
maybeLeft (Left a) = Just a
maybeLeft _ = Nothing

fillLeft :: a -> Maybe b -> Either a b
fillLeft _ (Just b) = Right b
fillLeft a Nothing = Left a

processResult :: FDB.Recipe -> FormResult RecipeFormReturn -> Either [Text] FDB.Recipe
processResult baseRecipe
      (FormSuccess (name, category,
        isFolder, maybeBook, maybePage, maybeUrl,
        maybeRating, maybeComments, ingredients)) = assembleParts eitherSource
  where
    eitherSource = fillLeft "Invalid recipe source specification" $
      constructSource isFolder maybeBook maybePage maybeUrl

    assembleParts (Right source) = Right $ baseRecipe {
        FDB.recipe_name = FDB.Name name,
        FDB.recipe_category = FDB.Category category,
        FDB.recipe_source = source,
        FDB.recipe_rating = constructRating maybeRating,
        FDB.recipe_comments = fromMaybe "" (fmap unTextarea maybeComments),
        FDB.recipe_ingredients = FDB.Ingredients (fromMaybe [] ingredients)
      }
    assembleParts es = Left $ catMaybes [maybeLeft es]

processResult _ _ = Left ["Failed to process form"]

postRecipeForm :: RecipeFormType -> Text -> Maybe Text -> Handler Html
postRecipeForm formType title priorName = do
    App {..} <- getYesod
    recipeSet <- liftIO (FDB.getRecipes appDatabase)
    recipeCategories <- liftIO (FDB.getCategories appDatabase)

    let maybeRecipe = priorName >>= \ name -> IxSet.getOne (recipeSet @= FDB.Name name)
    let baseRecipe = fromMaybe FDB.emptyRecipe maybeRecipe

    -- Field defaults don't matter for a failed post
    ((result, formWidget), formEnctype) <- runFormPost (recipeForm recipeCategories Nothing)

    case processResult baseRecipe result of
      Right recipe -> do
        let indexName = fromMaybe (getName recipe) priorName
        liftIO (FDB.setRecipe appDatabase indexName recipe)
        redirect RecipesR

      Left errorMsgs -> do
        defaultLayout $ do
            setTitle (toHtml title)
            $(widgetFile "recipe-editor")

recipeForm :: [Text] -> Maybe FDB.Recipe -> Form RecipeFormReturn
recipeForm categories maybeRecipe = renderBootstrap3 BootstrapBasicForm $ (,,,,,,,,)
    <$> nameField
    <*> categoryField
    <*> folderField
    <*> bookNameField
    <*> bookPageField
    <*> addressField
    <*> ratingField
    <*> commentsField
    <*> ingredientsField
  where
    nameField = areq
      textField
      (bfs ("Name" :: Text))
      (fmap getName maybeRecipe)

    categoryField = areq
      (selectFieldList (map dupe categories))
      (bfs ("Category" :: Text)) -- TODO: Add the 'mutiple' parameter with no value, might involve customising Yesod Forms
      (fmap getCategory maybeRecipe)

    folderField = areq
      checkBoxField
      (bfs ("Folder" :: Text))
      (fmap getIsFolder maybeRecipe)

    bookNameField = aopt
      textField
      (bfs ("Book" :: Text))
      (fmap getMaybeBook maybeRecipe)

    bookPageField = aopt
      intField
      (bfs ("Page" :: Text))
      (fmap getMaybePage maybeRecipe)

    addressField = aopt
      urlField
      (bfs ("Web address" :: Text))
      (fmap getMaybeUrl maybeRecipe)

    ratingField = aopt
      intField
      (bfs ("Rating" :: Text))
      (fmap getMaybeRating maybeRecipe)

    -- General properties ??? Perhaps just do comma separated with = for now

    commentsField = aopt
      textareaField
      (bfs ("Comments" :: Text))
      (Just $ fmap (Textarea . getComments) maybeRecipe)

    ingredientsField = aopt
      multiTDTField
      (bfs ("Ingredients" :: Text))
      (Just $ fmap getIngredients maybeRecipe)

    -- TODO: Allow creation of new categories
    -- e.g. http://www.tutorialrepublic.com/twitter-bootstrap-tutorial/bootstrap-typeahead.php
    -- TODO: Suggest book names similarly
