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
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
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

-- | Represents a top-level post in the Culture War Roundeup thread
data CWTop = CWTop
  { cWTopKind :: Text,
    cWTopId :: Text,
    cWTopAuthor :: Text,
    cWTopCreatedUtc :: Integer,
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
    let pats = [((), "*.json")]
        ignorePats = [".*"]
    FileSystem.mountOnLVar "." pats ignorePats model def $ \() fp action -> do
      case fp of
        "CWR-sanitizied.json" -> do
          case action of
            FileSystem.Update () -> do
              log $ "Reading " <> toText fp
              liftIO (eitherDecodeFileStrict @[CWTop] fp) >>= \case
                Left err -> error $ show err
                Right cwtops ->
                  pure $ \m -> m {modelCWTops = cwtops}
            FileSystem.Delete ->
              pure id
        _ ->
          pure id

render :: Ema.CLI.Action -> Model -> Route -> LByteString
render emaAction model Index =
  Tailwind.layout emaAction (H.title "TheMotte overview" >> H.base ! A.href "/") $
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
                " on "
                H.a ! A.class_ "text-xs text-blue-600 hover:underline" ! A.target "blank" ! A.href (H.toValue url) $ do
                  H.span $ H.toHtml $ show @Text . posixSecondsToUTCTime . fromInteger $ cWTopCreatedUtc
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
