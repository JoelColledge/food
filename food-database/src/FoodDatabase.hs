{-# LANGUAGE TypeFamilies, DeriveDataTypeable, OverloadedStrings, ViewPatterns, TemplateHaskell #-}
module FoodDatabase (
    DatabaseContext,
    open,
    close,
    getRecipes,
    ID,
  ) where

import Data.Acid

import Control.Arrow(second)
import Control.Monad(when)
import Control.Monad.State(get, put)
import Control.Monad.Reader(ask)
import Control.Applicative( (<$>) )
import System.Environment(getArgs)
import Data.List(transpose)
import Data.Maybe(maybeToList)
import Data.IxSet(Indexable(empty), IxSet, ixSet, ixFun, insert)
import Data.Ratio( (%) , Rational)
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

newtype Ratings = Ratings { getRatings :: [Int] } deriving (Eq, Ord, Show, Typeable)
$(deriveSafeCopy 0 'base ''Ratings)

newtype Properties = Properties { getProperties :: Map Text Text } deriving (Eq, Ord, Show, Typeable)
$(deriveSafeCopy 0 'base ''Properties)

data Recipe = Recipe {
    recipe_id :: ID,
    recipe_name :: Name,
    recipe_category :: Category,
    recipe_source :: RecipeSource,
    recipe_ratings :: Ratings,
    recipe_properties :: Properties,
    recipe_comments :: Text
  } deriving (Eq, Ord, Show, Typeable)
$(deriveSafeCopy 0 'base ''Recipe)

newtype Rating = Rating { getRating :: Rational } deriving (Eq, Ord, Show, Typeable)
computeRating :: Ratings -> Rating
computeRating (length . getRatings -> 0) = Rating $ 0
computeRating (getRatings -> ratings) = Rating $
  fromIntegral (sum ratings) %
    fromIntegral (length ratings)

instance Indexable Recipe where
  empty = ixSet [
      ixFun ((:[]) . recipe_id),
      ixFun ((:[]) . recipe_name),
      ixFun ((:[]) . recipe_category),
      ixFun ((:[]) . recipe_source),
      ixFun ((:[]) . computeRating . recipe_ratings)
    ]

data FoodDatabase = FoodDatabase {
    db_recipes :: IxSet Recipe
  } deriving (Show)

$(deriveSafeCopy 0 'base ''FoodDatabase)

type DatabaseContext = AcidState FoodDatabase

-- Transactions are defined to run in either the 'Update' monad
-- or the 'Query' monad.

addRecipe :: Recipe -> Update FoodDatabase ()
addRecipe recipe = do
  FoodDatabase recipes <- get
  put $ FoodDatabase (insert recipe recipes)

queryRecipes :: Query FoodDatabase (IxSet Recipe)
queryRecipes = do
  FoodDatabase recipes <- ask
  return $ recipes

-- Defines @QueryRecipes@ and @AddRecipe@.
$(makeAcidic ''FoodDatabase ['queryRecipes, 'addRecipe])

open :: IO DatabaseContext
open = do
  database <- openLocalStateFrom "myDatabase/" (FoodDatabase empty)
  return database

close :: DatabaseContext -> IO ()
close database = closeAcidState database

getRecipes :: DatabaseContext -> IO (IxSet Recipe)
getRecipes database = do
  recipes <- query database QueryRecipes
  return recipes

--
-- Pretty printing
--
recipeToCells :: Recipe -> [Text]
recipeToCells recipe =
  map (\ f -> f recipe) [T.pack . show . getID . recipe_id,
    getName . recipe_name,
    getCategory . recipe_category,
    T.pack . show . recipe_source,
    T.pack . show . getRatings . recipe_ratings,
    T.pack . show . getProperties . recipe_properties,
    recipe_comments]

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

readSource :: Text -> Text -> RecipeSource
readSource (T.uncons -> Nothing) _ = RecipeUnknown
readSource (T.unpack -> "F") _ = RecipeFolder
readSource sourceText pageText =
  RecipeBook (trimSpace sourceText) (maybeDecimal pageText)

maybeDecimal :: Integral a => Text -> Maybe a
maybeDecimal text = squashLeft $ do
  (val,_) <- T.decimal text
  return val

readProperties :: Text -> Text -> Properties
readProperties prepMinsText totalMinsText = Properties $ Map.fromList $
  filter (not . T.null . snd)
    [("Prep minutes", prepMinsText), ("Total minutes", totalMinsText)]

readCsvRecipe :: Text -> Either String Recipe
readCsvRecipe line = case csvLineFields line of
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
        recipe_category = Category category,
        recipe_source = readSource sourceText pageText,
        recipe_ratings = Ratings $ maybeToList $ maybeDecimal ratingText,
        recipe_properties = readProperties prepMinsText totalMinsText,
        recipe_comments = comments})
  otherwise -> Left "Failed to match fields"

importCsvRecipes :: DatabaseContext -> FilePath -> IO ()
importCsvRecipes database path = do
  file <- TL.readFile path
  mapM_ (addRecipeLine . TL.toStrict) (TL.lines file)
  where
    addRecipeLine line = case readCsvRecipe line of
      Right recipe -> do
--         putStrLn $ "Adding recipe: " ++ (show recipe)
        update database (AddRecipe recipe)
      Left errorMsg -> putStrLn (errorMsg ++ " (" ++ take 40 (T.unpack line) ++ ")")
