{-# LANGUAGE TypeFamilies, DeriveDataTypeable, ViewPatterns, TemplateHaskell #-}
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
import Data.SafeCopy
import Data.Typeable(Typeable)

import Data.Text(Text)
import qualified Data.Text as T(
  cons, uncons, break, drop, dropWhile, dropWhileEnd, pack, unpack, length,
  append, replicate, intercalate, unlines)
import qualified Data.Text.Read as T(decimal)
import qualified Data.Text.IO as T(putStrLn)
import qualified Data.Text.Lazy as TL(toStrict, lines)
import qualified Data.Text.Lazy.IO as TL(readFile)

-- TODO: List (Ingredient, Maybe Quantity)

data RecipeSource = RecipeFolder
                  | RecipeBook Text (Maybe Int)
                  | RecipeOnline Text
                  | RecipeUnknown
  deriving (Eq, Ord, Show)

$(deriveSafeCopy 0 'base ''RecipeSource)

data Recipe = Recipe {
    recipe_id :: Int,
    recipe_name :: Text,
    recipe_category :: Text,
    recipe_source :: RecipeSource,
    recipe_rating :: [Int],
    recipe_prepMins :: Maybe Int,
    recipe_totalMins :: Maybe Int,
    recipe_comments :: Text
  } deriving (Eq, Ord, Show, Typeable)

newtype ID = ID Int
  deriving (Eq, Ord, Show, Typeable)

idIndexes :: Recipe -> [ID]
idIndexes recipe = [ID $ recipe_id recipe]

instance Indexable Recipe where
  empty = ixSet [ ixFun idIndexes ]

$(deriveSafeCopy 0 'base ''Recipe)

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
  map (\ f -> f recipe) [T.pack . show . recipe_id,
    recipe_name,
    recipe_category,
    T.pack . show . recipe_source,
    T.pack . show . recipe_rating,
    T.pack . show . recipe_prepMins,
    T.pack . show . recipe_totalMins,
    recipe_comments]

columnWidths :: [[Text]] -> [Int]
columnWidths cols = map (maximum . map (T.length)) (transpose cols)

showCell :: Text -> Int -> Text
showCell text width = T.append text
  (T.replicate (width - T.length text) (T.pack " "))

showRow :: [(Text, Int)] -> Text
showRow cells = T.intercalate (T.pack " | ") $ map (uncurry showCell) cells

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
        recipe_id = idVal,
        recipe_name = name,
        recipe_category = category,
        recipe_source = readSource sourceText pageText,
        recipe_rating = maybeToList $ maybeDecimal ratingText,
        recipe_prepMins = maybeDecimal prepMinsText,
        recipe_totalMins = maybeDecimal totalMinsText,
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
