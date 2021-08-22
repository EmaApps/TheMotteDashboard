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
import Data.Time.Clock.POSIX (getCurrentTime, posixSecondsToUTCTime)
import Ema (Ema (..))
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.FileSystem as FileSystem
import qualified Ema.Helper.Tailwind as Tailwind
import System.Directory
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
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

-- | Represents a reddit post
data Post = Post
  { postKind :: Text,
    postId :: Text,
    postAuthor :: Text,
    postCreatedUtc :: Integer,
    postBody :: Text,
    postPermalink :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Post where
  parseJSON = genericParseJSONStripType

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
main = do
  -- Nix bundle CWD hack
  contentDir <- fromMaybe "." <$> lookupEnv "NIX_BUNDLE_CWD"
  putStrLn $ "CWD = " <> contentDir
  withCurrentDirectory contentDir $ do
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
  let now = unsafePerformIO getCurrentTime
  Tailwind.layout emaAction (H.title "TheMotte overview" >> extraHead emaAction) $
    H.div ! A.class_ "container mx-auto" $ do
      let routeBg = \case
            R_Index -> ""
            R_CW -> "bg-red-50"
            R_WW -> "bg-green-50"
            R_SQ -> "bg-gray-50"
            R_FF -> "bg-purple-100"
      H.div ! A.class_ "my-6 p-4" $ do
        H.div ! A.class_ "flex items-center justify-center" $ do
          H.div ! A.class_ "text-sm text-gray-400" $ do
            "Generated on "
            H.toHtml $ show @Text now
        case r of
          R_Index -> do
            H.div ! A.class_ "flex flex-wrap items-stretch" $ do
              forM_ [R_CW, R_WW, R_FF, R_SQ] $ \sectionR ->
                H.div ! A.class_ "w-full md:w-1/2 xl:w-1/4 overflow-hidden flex-grow" $ do
                  H.div ! A.class_ (routeBg sectionR <> " my-2 mx-2 p-2 rounded") $
                    renderSection sectionR
          _ -> do
            H.div ! A.class_ "my-2" $ do
              routeElem R_Index "View All"
            renderSection r
  where
    renderSection sectionRoute = do
      let (rName, rContent) = case sectionRoute of
            R_CW -> ("CW", modelCWPosts model)
            R_WW -> ("WW", modelWWPosts model)
            R_SQ -> ("SQ", modelSQPosts model)
            R_FF -> ("FF", modelFFPosts model)
            R_Index -> error "Bad route passed" -- FIXME: hack
      H.h1 ! A.class_ "text-5xl font-bold" $ do
        H.a ! routeHref sectionRoute $ do
          rName
          " - recent"
      H.ul $
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

    routeElem r' w =
      H.a ! A.class_ "text-blue-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl model r')
