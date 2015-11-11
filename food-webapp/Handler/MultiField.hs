module Handler.MultiField(
    multiTDTField,
  ) where

import Import

import qualified Data.Text.Read
import Data.Text as T ( Text, append, concat, cons, head
                      , intercalate, isPrefixOf, null, unpack, pack, splitOn
                      )
import qualified Data.Text as T (drop, dropWhile)

multiParseHelper :: (Monad m, RenderMessage site FormMessage)
               => ([Text] -> Either FormMessage a)
               -> [Text] -> [FileInfo] -> m (Either (SomeMessage site) (Maybe a))
multiParseHelper _ [] _ = return $ Right Nothing
multiParseHelper f xs _ = return $ either (Left . SomeMessage) (Right . Just) $ f xs

parseValsWithoutTemplate :: [Text] -> Either FormMessage [(Text, Maybe Double, Text)]
parseValsWithoutTemplate = parseVals . drop 3

parseVals :: [Text] -> Either FormMessage [(Text, Maybe Double, Text)]
parseVals [] = Right []
parseVals [_] = Left (MsgInputNotFound "Unexpected number of inputs found")
parseVals [_,_] = Left (MsgInputNotFound "Unexpected number of inputs found")
parseVals (x:y:z:xs) = do
    fstval <- Right x
    sndval <- parseDouble y
    thdval <- Right z
    rest <- parseVals xs
    return ((fstval, sndval, thdval) : rest)

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

multiTDTField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m [(Text, Maybe Double, Text)]
multiTDTField = Field
    { fieldParse = multiParseHelper $ parseValsWithoutTemplate
    , fieldView = \theId name attrs val isReq -> do
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.8.3/jquery.min.js"
        $(widgetFile "multi-text-double") -- TODO rename
    , fieldEnctype = UrlEncoded
    }
  where
    preFill = either (const []) id
    showQuantity = fromMaybe "" . fmap show
