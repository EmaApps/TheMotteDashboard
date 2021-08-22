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

-- | Represents a top-level post in the Culture War Roundup thread
data Post = Post
  { postKind :: Text,
    postId :: Text,
    postAuthor :: Text,
    postCreatedUtc :: Integer,
    postBody :: Text,
    postPermalink :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Post where parseJSON = genericParseJSONStripType

data Model = Model
  { modelCWPosts :: [Post]
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
              liftIO (eitherDecodeFileStrict @[Post] fp) >>= \case
                Left err -> error $ show err
                Right posts ->
                  pure $ \m -> m {modelCWPosts = posts}
            FileSystem.Delete ->
              pure id
        _ ->
          pure id

render :: Ema.CLI.Action -> Model -> Route -> LByteString
render emaAction model Index =
  Tailwind.layout emaAction (H.title "TheMotte overview" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "mt-8 p-2" $ do
        H.h1 ! A.class_ "text-5xl font-bold" $ do
          H.a ! A.href "https://old.reddit.com/r/TheMotte/" $ "r/TheMotte CW"
          " - recent"
        H.ul ! A.class_ "" $
          forM_ (modelCWPosts model) $ \Post {..} ->
            H.li ! A.class_ "mt-4" $ do
              H.div ! A.class_ "text-sm" $ do
                H.code $ do
                  "u/"
                  H.toHtml postAuthor
                let url = "http://old.reddit.com" <> postPermalink
                " on "
                H.a ! A.class_ "text-xs text-black-600 underline" ! A.target "blank" ! A.href (H.toValue url) $ do
                  H.span $ H.toHtml $ show @Text . posixSecondsToUTCTime . fromInteger $ postCreatedUtc
              H.blockquote ! A.class_ "mt-2 ml-2 pl-2 border-l-2" $ do
                let n = 80
                    nn = 400
                H.span ! A.class_ "font-bold" $ H.toHtml $ T.take n postBody
                H.span ! A.class_ "text-gray-500" $ do
                  H.toHtml $ T.take nn $ T.drop n postBody
                  "..."
        H.div ! A.class_ "mt-8 text-gray-300" $ do
          routeElem Index "Debug"
          H.pre $ H.toHtml (shower model)
  where
    routeElem r' w =
      H.a ! A.class_ "text-blue-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl model r')
