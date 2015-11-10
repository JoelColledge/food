module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              bfs)

import qualified Data.Text.Read
import Data.Text as T ( Text, append, concat, cons, head
                      , intercalate, isPrefixOf, null, unpack, pack, splitOn
                      )
import qualified Data.Text as T (drop, dropWhile)

import qualified FoodDatabase as FDB

getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let imported = False

    let debugText = "" :: String

    defaultLayout $ do
        setTitle "Colledge Food"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm

    App {..} <- getYesod
    liftIO (FDB.importFullRecipes appDatabase "Sources.csv" "Categories.csv" "Recipes.csv")

    let imported = True

    let debugText = show result

    defaultLayout $ do
        setTitle "Colledge Food"
        $(widgetFile "homepage")

multiParseHelper :: (Monad m, RenderMessage site FormMessage)
               => ([Text] -> Either FormMessage a)
               -> [Text] -> [FileInfo] -> m (Either (SomeMessage site) (Maybe a))
multiParseHelper _ [] _ = return $ Right Nothing
multiParseHelper f xs _ = return $ either (Left . SomeMessage) (Right . Just) $ f xs

-- eitherPair :: Either a b -> Either a c -> Either a (b,c)
-- eitherPair (Left s) _ = Left s
-- eitherPair _ (Left s) = Left s
-- eitherPair (Right sf) (Right ss) = Right (sf,ss)

parseValsWithoutTemplate :: [Text] -> Either FormMessage [(Text, Maybe Double)]
parseValsWithoutTemplate = parseVals . drop 2

parseVals :: [Text] -> Either FormMessage [(Text, Maybe Double)]
parseVals [] = Right []
parseVals [_] = Left (MsgInputNotFound "Odd number of inputs found")
parseVals (x:y:xs) = do
    fstval <- Right x
    sndval <- parseDouble y
    rest <- parseVals xs
    return ((fstval, sndval) : rest)

-- | Adds a '0' to some text so that it may be recognized as a double.
--   The read ftn does not recognize ".3" as 0.3 nor "-.3" as -0.3, so this
--   function changes ".xxx" to "0.xxx" and "-.xxx" to "-0.xxx"

prependZero :: Text -> Text
prependZero t0 = if T.null t1
                 then t1
                 else if T.head t1 == '.'
                      then '0' `T.cons` t1
                      else if "-." `T.isPrefixOf` t1
                           then "-0." `T.append` (T.drop 2 t1)
                           else t1

  where t1 = T.dropWhile ((==) ' ') t0

parseDouble :: Text -> Either FormMessage (Maybe Double)
parseDouble "" = Right Nothing
parseDouble s =
    case Data.Text.Read.double (prependZero s) of
        Right (a, "") -> Right (Just a)
        _ -> Left $ MsgInvalidNumber s

multiTextDoubleField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m [(Text, Maybe Double)]
multiTextDoubleField = Field
    { fieldParse = multiParseHelper $ parseValsWithoutTemplate
    , fieldView = \theId name attrs val isReq -> do
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.8.3/jquery.min.js"
        $(widgetFile "multi-text-double")
    , fieldEnctype = UrlEncoded
    }

sampleForm :: Form [(Text, Maybe Double)]
sampleForm = renderBootstrap3 BootstrapBasicForm $
    areq
      multiTextDoubleField
      (bfs ("Ingredients" :: Text))
      Nothing
