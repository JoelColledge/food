module FoodBackup (
    backup,
  ) where

import Data.Acid

import Data.IxSet(toList)
import qualified Data.Text.IO as T(writeFile)
import Data.Time.Clock(getCurrentTime)
import Data.Time.Format(formatTime)
import System.Locale(defaultTimeLocale)

import System.Process

import FoodDatabase

backup :: Context -> IO ()
backup db = do
  now <- getCurrentTime

  -- Pretty print and send to dropbox
  recipeSet <- getRecipes db
  let dumpFileName = formatTime defaultTimeLocale "recipesdump--%F--%H-%M-%S" now
  T.writeFile dumpFileName (showRecipeTable (toList recipeSet))
  callProcess "./dropbox_upload.sh" [dumpFileName]

  -- acid-state backup
  createCheckpoint db
  createArchive db
  let archiveFileName = formatTime defaultTimeLocale "archive--%F--%H-%M-%S.zip" now
  callProcess "zip" ["-r", archiveFileName, "food-db/Archive"]
  callProcess "./dropbox_upload.sh" [archiveFileName]

  -- TODO: Check success and clear Archive

  return ()
