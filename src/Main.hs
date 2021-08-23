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
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getCurrentTime, posixSecondsToUTCTime)
import Ema (Ema (..))
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.FileSystem as FileSystem
import qualified Ema.Helper.Tailwind as Tailwind
import System.Directory (withCurrentDirectory)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data MotteSticky
  = MS_CultureWar
  | MS_WellnessWednesday
  | MS_FridayFun
  | MS_SmallScaleQuestions
  deriving (Eq, Show, Enum, Bounded)

motteStickyName :: MotteSticky -> Text
motteStickyName = \case
  MS_CultureWar -> "CW"
  MS_WellnessWednesday -> "WW"
  MS_FridayFun -> "FF"
  MS_SmallScaleQuestions -> "SQ"

-- | Inverse of `motteStickyName`
readMotteSticky :: Text -> Maybe MotteSticky
readMotteSticky (T.toUpper -> s) =
  let nameMap = Map.fromList $ [minBound .. maxBound] <&> \ms -> (motteStickyName ms, ms)
   in Map.lookup (toText s) nameMap

data Route
  = R_Index
  | R_MotteSticky MotteSticky
  deriving (Eq, Show)

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
    modelFFPosts :: [Post],
    modelSQPosts :: [Post]
  }
  deriving (Show)

modelSetPosts :: MotteSticky -> [Post] -> Model -> Model
modelSetPosts ms xs model = case ms of
  MS_CultureWar -> model {modelCWPosts = xs}
  MS_WellnessWednesday -> model {modelWWPosts = xs}
  MS_FridayFun -> model {modelFFPosts = xs}
  MS_SmallScaleQuestions -> model {modelSQPosts = xs}

modelGetPosts :: Model -> MotteSticky -> [Post]
modelGetPosts Model {..} = \case
  MS_CultureWar -> modelCWPosts
  MS_WellnessWednesday -> modelWWPosts
  MS_FridayFun -> modelFFPosts
  MS_SmallScaleQuestions -> modelSQPosts

instance Default Model where
  def = Model mempty mempty mempty mempty

instance Ema Model Route where
  encodeRoute _model =
    \case
      R_Index -> "index.html"
      R_MotteSticky ms -> toString $ T.toLower (motteStickyName ms) <> ".html"
  decodeRoute _model = \case
    "index.html" -> Just R_Index
    (T.stripSuffix ".html" . toText -> Just baseName) ->
      R_MotteSticky <$> readMotteSticky baseName
    _ -> Nothing
  allRoutes _ =
    R_Index : (R_MotteSticky <$> [minBound .. maxBound])

log :: MonadLogger m => Text -> m ()
log = logInfoNS "TheMotteDashboard"

logD :: MonadLogger m => Text -> m ()
logD = logDebugNS "TheMotteDashboard"

main :: IO ()
main = do
  -- Nix bundle CWD hack
  contentDir <- fromMaybe "." <$> lookupEnv "NIX_BUNDLE_CWD"
  putStrLn $ "CWD = " <> contentDir
  withCurrentDirectory contentDir appMain

appMain :: IO ()
appMain = do
  Ema.runEma (\act m -> Ema.AssetGenerated Ema.Html . render act m) $ \_act model -> do
    let pats = [((), "*.json")]
        ignorePats = [".*"]
    FileSystem.mountOnLVar "." pats ignorePats model def $ \() fp action -> do
      -- Consume foo-sanitizied.json
      case T.stripSuffix "-sanitizied.json" (toText fp) >>= readMotteSticky of
        Just ms ->
          case action of
            FileSystem.Update () -> do
              log $ "Reading " <> toText fp
              liftIO (eitherDecodeFileStrict @[Post] fp) >>= \case
                Left err -> error $ show err
                Right posts ->
                  pure $ modelSetPosts ms posts
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
  Tailwind.layout emaAction (H.title "r/TheMotte dashboard" >> extraHead emaAction) $
    H.main ! A.class_ "mx-auto" $ do
      H.div ! A.class_ "my-6 p-4" $ do
        H.div ! A.class_ "flex items-center justify-center" $ do
          H.div ! A.class_ "text-sm text-gray-400" $ do
            "Generated on "
            H.toHtml $ show @Text now
        case r of
          R_Index -> do
            H.div ! A.class_ "flex flex-wrap items-stretch" $ do
              forM_ [minBound .. maxBound] $ \sectionR ->
                H.div ! A.class_ "w-full md:w-1/2 xl:w-1/4 overflow-hidden flex-grow" $ do
                  H.div ! A.class_ ("bg-" <> sectionClr sectionR <> "-50 my-2 mx-2 p-2") $
                    renderSection sectionR
          R_MotteSticky ms -> do
            H.div ! A.class_ "my-2" $ do
              routeElem R_Index "View All"
            renderSection ms
  where
    sectionClr = \case
      MS_CultureWar -> "red"
      MS_WellnessWednesday -> "green"
      MS_SmallScaleQuestions -> "gray"
      MS_FridayFun -> "purple"
    renderSection motteSticky = do
      H.h1 ! A.class_ "text-4xl font-bold" $ do
        H.a ! routeHref (R_MotteSticky motteSticky) $ do
          H.toHtml $ motteStickyName motteSticky
          " - recent"
      H.ul $
        forM_ (modelGetPosts model motteSticky) $ \Post {..} -> do
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
              H.blockquote ! A.class_ ("mt-2 ml-2 pl-2 border-l-2 hover:border-" <> sectionClr motteSticky <> "-600") $ do
                let n = 80
                    nn = 200
                H.span ! A.class_ "font-semibold visited:font-normal" $ H.toHtml $ T.take n postBody
                H.span ! A.class_ "text-gray-500" $ do
                  H.toHtml $ T.take nn $ T.drop n postBody
                  "..."

    routeElem r' w =
      H.a ! A.class_ "text-blue-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl model r')
