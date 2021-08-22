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
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Route
  = R_Index
  | R_CW
  | R_WW
  | R_SQ
  | R_FF
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
  { modelCWPosts :: [Post],
    modelWWPosts :: [Post],
    modelSQPosts :: [Post],
    modelFFPosts :: [Post]
  }
  deriving (Show)

instance Default Model where
  def = Model mempty mempty mempty mempty

instance Ema Model Route where
  encodeRoute _model =
    \case
      R_Index -> "index.html"
      R_CW -> "cw.html"
      R_WW -> "ww.html"
      R_FF -> "ff.html"
      R_SQ -> "sq.html"
  decodeRoute _model = \case
    "index.html" -> Just R_Index
    "cw.html" -> Just R_CW
    "ww.html" -> Just R_WW
    "ff.html" -> Just R_FF
    "sq.html" -> Just R_SQ
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
      let mSetPosts = case fp of
            "CWR-sanitizied.json" -> Just $ \posts m -> m {modelCWPosts = posts}
            "WW-sanitizied.json" -> Just $ \posts m -> m {modelWWPosts = posts}
            "SQ-sanitizied.json" -> Just $ \posts m -> m {modelSQPosts = posts}
            "FF-sanitizied.json" -> Just $ \posts m -> m {modelFFPosts = posts}
            _ -> Nothing
      case mSetPosts of
        Just setPosts -> do
          case action of
            FileSystem.Update () -> do
              log $ "Reading " <> toText fp
              liftIO (eitherDecodeFileStrict @[Post] fp) >>= \case
                Left err -> error $ show err
                Right posts ->
                  pure $ setPosts posts
            FileSystem.Delete ->
              pure id
        Nothing ->
          pure id

extraHead :: Ema.CLI.Action -> H.Html
extraHead emaAction = do
  case emaAction of
    Ema.CLI.Generate _ ->
      H.base ! A.href "https://srid.github.io/TheMotteDashboard/"
    _ ->
      H.base ! A.href "/"

render :: Ema.CLI.Action -> Model -> Route -> LByteString
render emaAction model r = do
  Tailwind.layout emaAction (H.title "TheMotte overview" >> extraHead emaAction) $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "my-8 p-2" $ do
        case r of
          R_Index -> do
            H.ul $ do
              H.li $ routeElem R_CW "CW"
              H.li $ routeElem R_WW "WW"
              H.li $ routeElem R_FF "FF"
              H.li $ routeElem R_SQ "SQ"
          _ -> do
            let (rName, rContent) = case r of
                  R_CW -> ("CW", modelCWPosts model)
                  R_WW -> ("WW", modelWWPosts model)
                  R_SQ -> ("SQ", modelSQPosts model)
                  R_FF -> ("FF", modelFFPosts model)
                  R_Index -> ("", mempty) -- FIXME: hack
            routeElem R_Index "Home"
            H.h1 ! A.class_ "text-5xl font-bold" $ do
              H.a $ do
                "r/TheMotte " <> rName
              " - recent"
            H.ul ! A.class_ "" $
              forM_ rContent $ \Post {..} -> do
                let url = "http://old.reddit.com" <> postPermalink
                    goto = mconcat [A.target "blank", A.href (H.toValue url)]
                H.li ! A.class_ "mt-4" $ do
                  H.div ! A.class_ "text-sm" $ do
                    H.code $ do
                      "u/"
                      H.toHtml postAuthor
                    " on "
                    H.a ! A.class_ "text-xs text-black-600 underline" ! goto $ do
                      H.span $ H.toHtml $ show @Text . posixSecondsToUTCTime . fromInteger $ postCreatedUtc
                  H.a ! goto $
                    H.blockquote ! A.class_ "mt-2 ml-2 pl-2 border-l-2" $ do
                      let n = 80
                          nn = 400
                      H.span ! A.class_ "font-bold" $ H.toHtml $ T.take n postBody
                      H.span ! A.class_ "text-gray-500" $ do
                        H.toHtml $ T.take nn $ T.drop n postBody
                        "..."
  where
    -- H.pre $ H.toHtml (shower model)

    routeElem r' w =
      H.a ! A.class_ "text-blue-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl model r')
