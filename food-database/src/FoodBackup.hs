module FoodBackup (
    backup,
  ) where

import Data.IxSet(toList)
import qualified Data.Text.IO as T(writeFile)
import Data.Time.Clock(getCurrentTime)
import Data.Time.Format(formatTime)
import System.Locale(defaultTimeLocale)

import System.Process

import FoodDatabase

backup :: Context -> IO ()
backup db = do
  -- Pretty print and send to dropbox
  recipeSet <- getRecipes db
  now <- getCurrentTime
  let fileName = formatTime defaultTimeLocale "recipesdump--%F--%H-%M-%S" now
  T.writeFile fileName (showRecipeTable (toList recipeSet))
  _ <- createProcess (proc "./dropbox_upload.sh" [fileName])
  return ()

  -- TODO: acid-state backup
