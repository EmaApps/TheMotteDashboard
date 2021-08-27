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
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (getCurrentTime, posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
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
  | MS_BareLinkRepository
  | MS_WellnessWednesday
  | MS_FridayFun
  | MS_SmallScaleQuestions
  deriving (Eq, Show, Enum, Bounded)

motteStickyName :: MotteSticky -> Text
motteStickyName = \case
  MS_CultureWar -> "CW"
  MS_BareLinkRepository -> "BLR"
  MS_WellnessWednesday -> "WW"
  MS_FridayFun -> "FF"
  MS_SmallScaleQuestions -> "SQ"

motteStickyLongName :: MotteSticky -> Text
motteStickyLongName = \case
  MS_CultureWar -> "Culture War"
  MS_BareLinkRepository -> "Culture War Links"
  MS_WellnessWednesday -> "Wellness Wednesday"
  MS_FridayFun -> "Friday Fun"
  MS_SmallScaleQuestions -> "Small-Scale Question"

-- | Inverse of `motteStickyName`
readMotteSticky :: Text -> Maybe MotteSticky
readMotteSticky (T.toUpper -> s) =
  let nameMap = Map.fromList $ [minBound .. maxBound] <&> \ms -> (motteStickyName ms, ms)
   in Map.lookup (toText s) nameMap

data Route
  = R_Index
  | R_Timeline
  | R_MotteSticky MotteSticky
  | R_Users
  | R_User Text
  deriving (Eq, Show)

-- FIXME: A hack that can be removed by using inner ADT for user Route
isUserRoute :: Route -> Bool
isUserRoute = \case
  R_Users -> True
  R_User _ -> True
  _ -> False

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
    modelBLRPosts :: [Post],
    modelWWPosts :: [Post],
    modelFFPosts :: [Post],
    modelSQPosts :: [Post]
  }
  deriving (Show)

modelSetPosts :: MotteSticky -> [Post] -> Model -> Model
modelSetPosts ms xs model = case ms of
  MS_CultureWar -> model {modelCWPosts = xs}
  MS_BareLinkRepository -> model {modelBLRPosts = xs}
  MS_WellnessWednesday -> model {modelWWPosts = xs}
  MS_FridayFun -> model {modelFFPosts = xs}
  MS_SmallScaleQuestions -> model {modelSQPosts = xs}

modelGetPosts :: Model -> MotteSticky -> [Post]
modelGetPosts Model {..} = \case
  MS_CultureWar -> modelCWPosts
  MS_BareLinkRepository -> modelBLRPosts
  MS_WellnessWednesday -> modelWWPosts
  MS_FridayFun -> modelFFPosts
  MS_SmallScaleQuestions -> modelSQPosts

-- | Return all posts in reverse chronological order
modelGetPostTimeline :: Model -> [(MotteSticky, Post)]
modelGetPostTimeline model =
  sortOn (Down . postCreatedUtc . snd) $
    mconcat $
      [minBound .. maxBound] <&> \ms ->
        (ms,) <$> modelGetPosts model ms

modelGetUsers :: Model -> [(Text, [Post])]
modelGetUsers model =
  let m :: Map Text [Post] = Map.fromListWith (<>) $ modelGetPostTimeline model <&> (postAuthor . snd &&& one . snd)
   in Map.toAscList m

instance Default Model where
  def = Model mempty mempty mempty mempty mempty

instance Ema Model Route where
  encodeRoute _model =
    \case
      R_Index -> "index.html"
      R_Timeline -> "timeline.html"
      R_MotteSticky ms -> toString $ T.toLower (motteStickyName ms) <> ".html"
      R_Users -> "u.html"
      R_User name -> "u/" <> toString name <> ".html"
  decodeRoute _model = \case
    "index.html" -> Just R_Index
    "timeline.html" -> Just R_Timeline
    "u.html" -> Just R_Users
    (T.stripSuffix ".html" . toText -> Just baseName) ->
      case T.stripPrefix "u/" baseName of
        Just userName ->
          Just $ R_User userName
        Nothing ->
          R_MotteSticky <$> readMotteSticky baseName
    _ -> Nothing
  allRoutes model =
    mconcat
      [ [R_Index],
        R_MotteSticky <$> [minBound .. maxBound],
        R_User <$> (modelGetUsers model <&> fst)
      ]

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

extraHead :: H.Html
extraHead = do
  H.base ! A.href "/"
  -- TODO: until we get windicss compilation
  H.style
    " .extlink:visited { \
    \   color: dimgray; \
    \ } \
    \ .extlink:link { \
    \  font-weight: 600; \
    \ } "

headTitle :: Route -> H.Html
headTitle r = do
  let siteTitle = "r/TheMotte dashboard"
  case r of
    R_Index ->
      H.title $ H.toHtml siteTitle
    R_Timeline ->
      H.title $ H.toHtml $ "Timeline - " <> siteTitle
    R_MotteSticky ms ->
      H.title $ H.toHtml $ motteStickyLongName ms <> " - " <> siteTitle
    R_Users ->
      H.title $ H.toHtml $ "Users - " <> siteTitle
    R_User name ->
      H.title $ H.toHtml $ "u/" <> name <> " - " <> siteTitle

data ViewMode
  = ViewGrid
  | ViewFull
  deriving (Eq, Show)

render :: Ema.CLI.Action -> Model -> Route -> LByteString
render emaAction model r = do
  let now = unsafePerformIO getCurrentTime
  Tailwind.layout emaAction (headTitle r >> extraHead) $
    H.main ! A.class_ "mx-auto" $ do
      H.div ! A.class_ "my-2 p-4" $ do
        H.div ! A.class_ "flex items-center justify-center gap-4" $ do
          let linkBg tr = if tr == r || (tr == R_Users && isUserRoute r) then "bg-blue-300" else ""
              mkLink tr = H.a ! A.class_ (linkBg tr <> " px-1 py-0.5 rounded") ! routeHref tr
          mkLink R_Timeline "Timeline View"
          mkLink R_Index "Dashboard View"
          mkLink R_Users "User View"
        case r of
          R_Index -> do
            H.div ! A.class_ "flex flex-wrap items-stretch" $ do
              forM_ [minBound .. maxBound] $ \ms ->
                H.div ! A.class_ "w-full md:w-1/2 lg:w-1/3 xl:w-1/4 2xl:w-1/5 overflow-hidden flex-grow" $ do
                  H.div ! A.class_ ("bg-" <> sectionClr ms <> "-50 my-2 mx-2 p-2") $
                    renderSection ms ViewGrid
          R_Timeline -> do
            H.div ! A.class_ "my-2 p-2 container mx-auto" $
              renderTimeline $ modelGetPostTimeline model
          R_MotteSticky ms -> do
            H.div ! A.class_ ("bg-" <> sectionClr ms <> "-50 my-2 p-2 container mx-auto") $
              renderSection ms ViewFull
          R_Users -> do
            H.div ! A.class_ "my-2 p-2 container mx-auto" $ do
              H.div ! A.class_ "flex flex-col gap-2" $ do
                forM_ (modelGetUsers model & sortOn (Down . length . snd)) $ \(user, posts) -> do
                  H.div ! A.class_ "flex flex-nowrap items-center gap-2" $ do
                    renderPostAuthor user
                    H.span ! A.class_ "text-gray-500 text-xs font-mono" $ H.toHtml $ length posts
          R_User name -> do
            H.div ! A.class_ "my-2 p-2 container mx-auto" $ do
              renderRouteHeading R_Users "blue" $ H.toHtml $ "u/" <> name
              renderTimeline $
                modelGetPostTimeline model & filter ((== name) . postAuthor . snd)
        H.div ! A.class_ "flex items-center justify-center" $ do
          H.div ! A.class_ "text-sm text-gray-400" $ do
            "Generated on "
            renderTime now
        H.div ! A.class_ "flex items-center justify-center" $ do
          H.div ! A.class_ "text-sm text-gray-500" $ do
            H.p $ do
              "Powered by "
              H.a ! A.class_ "font-semibold" ! A.href "https://ema.srid.ca" $ "Ema"
              " ("
              H.a ! A.href "https://github.com/srid/TheMotteDashboard" $ "View Source"
              ")"
  where
    showTime :: UTCTime -> Text
    showTime =
      toText . formatTime defaultTimeLocale "%b %d, %R UTC"
    renderTime t = do
      H.span ! A.title (H.toValue $ show @Text t) $ H.toHtml $ showTime t
    sectionClr = \case
      MS_CultureWar -> "red"
      MS_BareLinkRepository -> "yellow"
      MS_WellnessWednesday -> "green"
      MS_SmallScaleQuestions -> "gray"
      MS_FridayFun -> "purple"
    renderSection motteSticky viewMode = do
      let otherRoute = case viewMode of
            ViewGrid -> R_MotteSticky motteSticky
            ViewFull -> R_Index
      H.h1 ! A.class_ ("py-1 text-2xl italic font-semibold font-mono border-b-2 bg-" <> sectionClr motteSticky <> "-200") $ do
        H.a ! routeHref otherRoute ! A.title "Switch View" ! A.class_ "flex items-center justify-center" $ do
          H.toHtml $ motteStickyLongName motteSticky
      H.ul $
        forM_ (modelGetPosts model motteSticky) $ \post@Post {..} -> do
          H.li ! A.class_ "mt-4" $ do
            H.div ! A.class_ "text-sm flex flex-row flex-nowrap justify-between pr-1" $ do
              H.div $
                renderPostAuthor postAuthor
              H.div $
                renderPostTime post
            H.div ! A.class_ "pt-2" $
              renderPostBody motteSticky viewMode post

    renderPostAuthor author =
      H.code $ do
        H.a ! routeHref (R_User author) $
          "u/" <> H.toHtml author
    renderRouteHeading headingR clr w =
      H.h1 ! A.class_ ("py-1 text-2xl italic font-semibold font-mono border-b-2 bg-" <> clr <> "-200") $ do
        H.a ! routeHref headingR ! A.title "Switch View" ! A.class_ "flex items-center justify-center" $ do
          w

    renderPostBody motteSticky viewMode p@Post {..} =
      H.blockquote ! A.class_ ("ml-2 pl-2 border-l-2 hover:border-" <> sectionClr motteSticky <> "-600") $ do
        let n = 80
            nn = if viewMode == ViewGrid then 200 else 700
        -- TODO: After moving to windicss, replace extlink with visited:text-gray-500
        H.a ! postLinkAttr p ! A.class_ "extlink" $ H.toHtml $ T.take n postBody
        H.a ! postLinkAttr p ! A.class_ "text-gray-500" $ do
          H.toHtml $ T.take nn $ T.drop n postBody
          "..."

    renderPostTime p@Post {..} =
      H.a ! A.class_ "text-xs text-gray-400" ! postLinkAttr p $ do
        renderTime $ posixSecondsToUTCTime . fromInteger $ postCreatedUtc

    renderTimeline :: [(MotteSticky, Post)] -> H.Html
    renderTimeline posts =
      forM_ posts $ \(ms, post@Post {..}) -> do
        H.div ! A.class_ ("p-2 flex flex-row bg-" <> sectionClr ms <> "-50") ! A.title (H.toValue $ motteStickyLongName ms) $ do
          H.div ! A.class_ "font-mono flex flex-col w-32" $ do
            renderPostTime post
            H.div ! A.class_ "mt-0.5 text-xs overflow-hidden text-gray-600" $ renderPostAuthor postAuthor
          H.div ! A.class_ "flex-1" $ renderPostBody ms ViewFull post

    postLinkAttr Post {..} =
      let url = "http://old.reddit.com" <> postPermalink
       in mconcat [A.target "blank", A.href (H.toValue url)]

    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl model r')
