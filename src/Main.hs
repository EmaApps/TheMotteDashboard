{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Logger
import Data.Aeson (FromJSON (parseJSON), eitherDecodeFileStrict)
import Data.Aeson.Options (genericParseJSONStripType)
import Data.Default (Default (..))
import qualified Data.Text as T
import Ema (Ema (..))
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.FileSystem as FileSystem
import qualified Ema.Helper.Tailwind as Tailwind
import Shower (shower)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Route
  = Index
  deriving (Show, Enum, Bounded)

data CWTop = CWTop
  { cWTopKind :: Text,
    cWTopId :: Text,
    cWTopAuthor :: Text,
    cWTopCreatedUtc :: Int,
    cWTopBody :: Text,
    cWTopPermalink :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON CWTop where parseJSON = genericParseJSONStripType

data Model = Model
  { modelCWTops :: [CWTop]
  }
  deriving (Show)

instance Default Model where
  def = Model mempty

instance Ema Model Route where
  encodeRoute _model =
    \case
      Index -> "index.html"
  decodeRoute _model = \case
    "index.html" -> Just Index
    _ -> Nothing

log :: MonadLogger m => Text -> m ()
log = logInfoNS "TheMotteDashboard"

logD :: MonadLogger m => Text -> m ()
logD = logDebugNS "TheMotteDashboard"

main :: IO ()
main =
  Ema.runEma (\act m -> Ema.AssetGenerated Ema.Html . render act m) $ \_act model -> do
    -- This is the place where we can load and continue to modify our "model".
    -- You will use `LVar.set` and `LVar.modify` to modify the model.
    --
    -- It is a run in a (long-running) thread of its own.
    --
    -- We use the FileSystem helper to directly "mount" our files on to the
    -- LVar.
    let pats = [((), "*.json")]
        ignorePats = [".*"]
    FileSystem.mountOnLVar "." pats ignorePats model def $ \() fp action -> do
      case action of
        FileSystem.Update () -> do
          case fp of
            "CWR-sanitizied.json" -> do
              liftIO (eitherDecodeFileStrict @[CWTop] fp) >>= \case
                Left err -> error $ show err
                Right cwtops ->
                  pure $ \m -> m {modelCWTops = cwtops}
            _ ->
              pure id
        FileSystem.Delete ->
          pure id

render :: Ema.CLI.Action -> Model -> Route -> LByteString
render emaAction model Index =
  Tailwind.layout emaAction (H.title "Basic site" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "mt-8 p-2" $ do
        "You are on the "
        routeElem Index "index page"
        H.ul ! A.class_ "list-disc" $
          forM_ (modelCWTops model) $ \CWTop {..} ->
            H.li $ do
              H.div $ do
                "u/"
                H.em $ H.toHtml cWTopAuthor
                let url = "http://old.reddit.com" <> cWTopPermalink
                H.a ! A.class_ "text-blue-600 hover:underline" ! A.href (H.toValue url) $ do
                  " on "
                  H.span $ H.toHtml cWTopCreatedUtc
              H.blockquote $ do
                let n = 80
                H.span ! A.class_ "font-bold" $ H.toHtml $ T.take n cWTopBody
                H.span ! A.class_ "text-gray-500" $ H.toHtml $ T.drop n cWTopBody
              "..."
        H.pre $ H.toHtml (shower model)
  where
    routeElem r' w =
      H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl model r')
