{-# LANGUAGE TypeFamilies, DeriveDataTypeable, OverloadedStrings, ViewPatterns, TemplateHaskell #-}
module FoodDatabase (
    Context,
    open,
    close,
    getRecipes,
    setRecipe,
    getCategories,
    ID(..),
    Name(..),
    Category(..),
    RecipeSource(..),
    Rating(..),
    Properties(..),
    Ingredients(..),
    Recipe(..),

    emptyRecipe,
    maybeToRating,
    ratingToMaybe,

    printRecipeTable,

    importFullRecipes,
  ) where

import Debug.Trace(trace)

import Prelude

import Data.Acid

import Control.Arrow(second)
-- import Control.Monad(when)
import Control.Monad.State(get, put)
import Control.Monad.Reader(ask)
-- import Control.Applicative( (<$>) )
-- import System.Environment(getArgs)
import Data.IxSet(Indexable(empty), IxSet, ixSet, ixFun, insert, updateIx, toList)
import Data.List(nub, transpose)
import Data.Maybe(catMaybes)
-- import Data.Ratio( (%) , Rational)
import Data.SafeCopy
import Data.Typeable(Typeable)

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

import Data.Text(Text)
import qualified Data.Text as T(
  cons, uncons, break, drop, dropWhile, dropWhileEnd, pack, unpack, length,
  append, replicate, intercalate, unlines, null)
import qualified Data.Text.Read as T(decimal)
import qualified Data.Text.IO as T(putStrLn)
import qualified Data.Text.Lazy as TL(toStrict, lines)
import qualified Data.Text.Lazy.IO as TL(readFile)

-- TODO: List (Ingredient, Maybe Quantity)

newtype ID = ID { getID :: Int } deriving (Eq, Ord, Show, Typeable)
$(deriveSafeCopy 0 'base ''ID)

newtype Name = Name { getName :: Text } deriving (Eq, Ord, Show, Typeable)
$(deriveSafeCopy 0 'base ''Name)

newtype Category = Category { getCategory :: Text } deriving (Eq, Ord, Show, Typeable)
$(deriveSafeCopy 0 'base ''Category)

data RecipeSource = RecipeFolder
                  | RecipeBook Text (Maybe Int)
                  | RecipeOnline Text
                  | RecipeUnknown
  deriving (Eq, Ord, Show, Typeable)
$(deriveSafeCopy 0 'base ''RecipeSource)

data Rating = Rating { getRating :: Int }
            | RatingNone deriving (Eq, Ord, Show, Typeable)
$(deriveSafeCopy 0 'base ''Rating)

newtype Properties = Properties { getProperties :: Map Text Text } deriving (Eq, Ord, Show, Typeable)
$(deriveSafeCopy 0 'base ''Properties)

newtype Ingredients = Ingredients { getIngredients :: [(Text, Maybe Double, Text)] } deriving (Eq, Ord, Show, Typeable)
$(deriveSafeCopy 0 'base ''Ingredients)

data Recipe = Recipe {
    recipe_id :: ID,
    recipe_name :: Name,
    recipe_category :: Category,
    recipe_source :: RecipeSource,
    recipe_rating :: Rating,
    recipe_properties :: Properties,
    recipe_comments :: Text,
    recipe_ingredients :: Ingredients
  } deriving (Eq, Ord, Show, Typeable)
$(deriveSafeCopy 0 'base ''Recipe)

instance Indexable Recipe where
  empty = ixSet [
      ixFun ((:[]) . recipe_id),
      ixFun ((:[]) . recipe_name),
      ixFun ((:[]) . recipe_category),
      ixFun ((:[]) . recipe_source),
      ixFun ((:[]) . recipe_rating)
    ]

type BookMap = Map Text Text

type CategoryMap = Map Text Text

data FoodDatabase = FoodDatabase {
    db_recipes :: IxSet Recipe
  } deriving (Show)

$(deriveSafeCopy 0 'base ''FoodDatabase)

type Context = AcidState FoodDatabase

-- Transactions are defined to run in either the 'Update' monad
-- or the 'Query' monad.

addRecipe :: Recipe -> Update FoodDatabase ()
addRecipe recipe = do
    foodDatabase <- get
    put $ foodDatabase { db_recipes = insert recipe (db_recipes foodDatabase) }

-- Update a recipe with a given name, or create it if none exists
updateRecipe :: Name -> Recipe -> Update FoodDatabase ()
updateRecipe name recipe = do
    foodDatabase <- get
    put $ foodDatabase { db_recipes = updateIx name recipe (db_recipes foodDatabase) }

queryRecipes :: Query FoodDatabase (IxSet Recipe)
queryRecipes = do
    foodDatabase <- trace "Asking for db" $ ask
    return $ db_recipes foodDatabase

-- Defines @QueryRecipes@, @AddRecipe@, @UpdateRecipe@
$(makeAcidic ''FoodDatabase [
    'queryRecipes, 'addRecipe, 'updateRecipe
  ])

open :: IO Context
open = do
    database <- openLocalStateFrom "myDatabase/" (FoodDatabase empty)
    return database

close :: Context -> IO ()
close database = closeAcidState database

getRecipes :: Context -> IO (IxSet Recipe)
getRecipes database = do
    recipes <- query database QueryRecipes
    return recipes

-- Set a recipe with a given name
setRecipe :: Context -> Text -> Recipe -> IO ()
setRecipe database name recipe = do
    update database (UpdateRecipe (Name name) recipe)

uniqueCategories :: IxSet Recipe -> [Text]
uniqueCategories recipeSet =
    nub $
      map (getCategory . recipe_category) $
        toList recipeSet

-- TODO: cache list?
getCategories :: Context -> IO [Text]
getCategories database = do
    recipeSet <- getRecipes database
    return $ uniqueCategories recipeSet

--
-- Helper functions
--
emptyRecipe :: Recipe
emptyRecipe = Recipe (ID 0) (Name "") (Category "")
    RecipeUnknown RatingNone (Properties Map.empty) "" (Ingredients [])

maybeToRating :: Maybe Int -> Rating
maybeToRating (Just a) = Rating a
maybeToRating Nothing = RatingNone

ratingToMaybe :: Rating -> Maybe Int
ratingToMaybe (Rating a) = Just a
ratingToMaybe RatingNone = Nothing

--
-- Pretty printing
--
recipeToCells :: Recipe -> [Text]
recipeToCells recipe =
    map (\ f -> f recipe) [T.pack . show . getID . recipe_id,
      getName . recipe_name,
      getCategory . recipe_category,
      T.pack . show . recipe_source,
      T.pack . show . recipe_rating,
      T.pack . show . getProperties . recipe_properties,
      recipe_comments,
      T.pack . show . getIngredients . recipe_ingredients]

columnWidths :: [[Text]] -> [Int]
columnWidths cols = map (maximum . map (T.length)) (transpose cols)

showCell :: Text -> Int -> Text
showCell text width = T.append text
  (T.replicate (width - T.length text) " ")

showRow :: [(Text, Int)] -> Text
showRow cells = T.intercalate " | " $ map (uncurry showCell) cells

showRecipeTable :: [Recipe] -> Text
showRecipeTable recipes = T.unlines allRows
  where
    allCells = map recipeToCells recipes
    widths = columnWidths allCells
    allRows = map showRow (map (flip zip widths) allCells)

printRecipeTable :: [Recipe] -> IO ()
printRecipeTable = T.putStrLn . showRecipeTable

--
-- CSV Parsing
--
csvLineFields :: Text -> [Text]
csvLineFields = csvLineFields' . T.cons ','
csvLineFields' :: Text -> [Text]
csvLineFields' (T.uncons -> Nothing) = []
csvLineFields' (T.uncons -> Just (',', cs)) = field : csvLineFields' rest
  where (field, rest) = csvField cs
csvLineFields' _ = error "Invalid CSV"

-- extracts a field and the remainder including separator
csvField :: Text -> (Text, Text)
csvField (T.uncons -> Just ('"', cs)) = second (T.drop 1) $ T.break (=='"') cs
csvField cs = T.break (==',') cs

squashLeft :: Either a b -> Maybe b
squashLeft (Left _) = Nothing
squashLeft (Right b) = Just b

trimSpace :: Text -> Text
trimSpace = T.dropWhile (==' ') . T.dropWhileEnd (==' ')

maybeDecimal :: Integral a => Text -> Maybe a
maybeDecimal text = squashLeft $ do
    (val,_) <- T.decimal text
    return val

readCsv :: FilePath -> (Text -> Either String a) -> IO [a]
readCsv path lineReader = do
    file <- TL.readFile path
    maybeLineValues <- mapM (maybeReadLine . TL.toStrict) (TL.lines file)
    return (catMaybes maybeLineValues)
  where
    maybeReadLine line = case lineReader line of
      Right lineValue -> return (Just lineValue)
      Left errorMsg -> do
        putStrLn (errorMsg ++ " (" ++ take 40 (T.unpack line) ++ ")")
        return Nothing

-- Recipes
readSource :: BookMap -> Text -> Text -> RecipeSource
readSource _ (T.uncons -> Nothing) _ = RecipeUnknown
readSource _ (T.unpack -> "F") _ = RecipeFolder
readSource bookMap sourceText pageText =
    RecipeBook (Map.findWithDefault bookCode bookCode bookMap) (maybeDecimal pageText)
  where bookCode = trimSpace sourceText

readProperties :: Text -> Text -> Properties
readProperties prepMinsText totalMinsText = Properties $ Map.fromList $
    filter (not . T.null . snd)
      [("Prep minutes", prepMinsText), ("Total minutes", totalMinsText)]

readCsvRecipe :: BookMap -> CategoryMap -> Text -> Either String Recipe
readCsvRecipe bookMap categoryMap line = case csvLineFields line of
    [idText,
      name,
      category,
      sourceText,
      pageText,
      ratingText,
      prepMinsText,
      totalMinsText,
      comments] -> do
        (idVal,_) <- T.decimal idText
        return (Recipe {
          recipe_id = ID idVal,
          recipe_name = Name name,
          recipe_category = Category (Map.findWithDefault category category categoryMap),
          recipe_source = readSource bookMap sourceText pageText,
          recipe_rating = maybeToRating $ maybeDecimal ratingText,
          recipe_properties = readProperties prepMinsText totalMinsText,
          recipe_comments = comments,
          recipe_ingredients = Ingredients [("Flour", Just 300, "g")]})
    _ -> Left "Failed to match recipe fields"

readCsvRecipes :: Context -> BookMap -> CategoryMap -> FilePath -> IO ()
readCsvRecipes database bookMap categoryMap path = do
    recipes <- readCsv path (readCsvRecipe bookMap categoryMap)
    mapM_ (update database . AddRecipe) recipes

-- Books
readCsvBook :: Text -> Either String (Text, Text)
readCsvBook line = case csvLineFields line of
    [bookCode,
      bookName] -> Right (bookCode, bookName)
    _ -> Left "Failed to match book fields"

readCsvBooks :: FilePath -> IO BookMap
readCsvBooks path =
    fmap Map.fromList $ readCsv path readCsvBook

-- Categories
readCsvCategory :: Text -> Either String (Text, Text)
readCsvCategory line = case csvLineFields line of
    [categoryCode,
      categoryName] -> Right (categoryCode, categoryName)
    _ -> Left "Failed to match category fields"

readCsvCategories :: FilePath -> IO CategoryMap
readCsvCategories path =
    fmap Map.fromList $ readCsv path readCsvCategory

-- Together
importFullRecipes :: Context -> FilePath -> FilePath -> FilePath -> IO ()
importFullRecipes database bookPath categoryPath recipePath = do
    bookMap <- readCsvBooks bookPath
    categoryMap <- readCsvCategories categoryPath
    readCsvRecipes database bookMap categoryMap recipePath
