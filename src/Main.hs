{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Logger (MonadLogger, logDebugNS, logInfoNS)
import Data.Aeson (FromJSON (parseJSON), eitherDecodeFileStrict, genericParseJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (UTCTime, utc, utcToZonedTime)
import Data.Time.Clock.POSIX (getCurrentTime, posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.RFC3339 (formatTimeRFC3339)
import Ema hiding (routeUrl)
import Optics.Core
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import qualified System.UnionMount as UM
import qualified Text.Atom.Feed as F
import qualified Text.Atom.Feed.Export as F (textFeed)
import qualified Text.Blaze.Html.Renderer.Utf8 as RU
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

motteStickyLongName :: MotteSticky -> Text
motteStickyLongName = \case
  MS_CultureWar -> "Culture War"
  MS_WellnessWednesday -> "Wellness Wednesday"
  MS_FridayFun -> "Friday Fun"
  MS_SmallScaleQuestions -> "Small-Scale Question"

-- | Inverse of `motteStickyName`
readMotteSticky :: Text -> Maybe MotteSticky
readMotteSticky (T.toUpper -> s) =
  let nameMap = Map.fromList $ [minBound .. maxBound] <&> \ms -> (motteStickyName ms, ms)
   in Map.lookup (toText s) nameMap

data AppRoute
  = AppRoute_Html Route
  | AppRoute_Atom ListingRoute
  deriving (Eq, Show)

data Route
  = R_Index
  | R_Users
  | R_Listing ListingRoute
  deriving (Eq, Show)

data ListingRoute
  = LR_Timeline
  | LR_MotteSticky MotteSticky
  | LR_User Text
  deriving (Eq, Show)

listingTitle :: ListingRoute -> Text
listingTitle = \case
  LR_Timeline -> "Timeline"
  LR_MotteSticky ms -> motteStickyLongName ms
  LR_User name -> "u/" <> name

listingUrl :: Prism' FilePath AppRoute -> ListingRoute -> Text
listingUrl rp lr =
  let path = routeUrl rp (AppRoute_Html $ R_Listing lr)
   in siteUrl <> path

routeUrl :: Prism' FilePath r -> r -> Text
routeUrl = Ema.routeUrlWith Ema.UrlPretty

-- FIXME: A hack that can be removed by using inner ADT for user Route
isUserRoute :: Route -> Bool
isUserRoute = \case
  R_Users -> True
  R_Listing (LR_User _) -> True
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
  parseJSON = genericParseJSON $ aesonPrefix camelCase

data Model = Model
  { modelCWPosts :: [Post],
    modelWWPosts :: [Post],
    modelFFPosts :: [Post],
    modelSQPosts :: [Post]
  }
  deriving (Show)

siteUrl :: F.URI
siteUrl = "https://themotte.srid.ca/"

postUrl :: Post -> Text
postUrl Post {..} =
  "http://old.reddit.com" <> postPermalink

postTime :: Post -> UTCTime
postTime =
  posixSecondsToUTCTime . fromInteger . postCreatedUtc

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

modelGetUserPosts :: Model -> Text -> [(MotteSticky, Post)]
modelGetUserPosts model name =
  modelGetPostTimeline model & filter ((== name) . postAuthor . snd)

modelListingFeedEntries :: Prism' FilePath AppRoute -> Model -> ListingRoute -> [F.Entry]
modelListingFeedEntries rp model = \case
  LR_Timeline ->
    modelGetPostTimeline model <&> \(ms, post) ->
      mkPostEntry post & assignMotteSticky ms
  LR_MotteSticky ms ->
    modelGetPosts model ms <&> \post ->
      mkPostEntry post
  LR_User name ->
    modelGetUserPosts model name <&> \(ms, post) ->
      mkPostEntry post & assignMotteSticky ms
  where
    assignMotteSticky ms entry =
      let catUrl = siteUrl <> routeUrl rp (AppRoute_Html $ R_Listing $ LR_MotteSticky ms)
       in entry
            { F.entryCategories = one $ F.Category (motteStickyLongName ms) (Just catUrl) (Just $ motteStickyName ms) mempty,
              F.entryTitle = F.TextString $ prefixed (motteStickyName ms) $ toText . F.txtToString $ F.entryTitle entry
            }
    mkPostEntry post =
      let itemTitle = T.take 80 $ postBody post
       in (F.nullEntry (postUrl post) (F.TextString itemTitle) (formatTimeRFC3339 $ utcToZonedTime utc $ postTime post))
            { F.entryContent = Just $ F.TextContent $ postBody post,
              F.entryAuthors = one $ F.nullPerson {F.personName = postAuthor post},
              F.entryLinks = one $ F.nullLink (postUrl post)
            }
    prefixed x s =
      "[" <> x <> "] " <> s

listingFeed :: Prism' FilePath AppRoute -> Model -> ListingRoute -> F.Feed
listingFeed rp model lr =
  let items = modelListingFeedEntries rp model lr
      lastUpdated = maybe "??" (F.entryUpdated . head) $ nonEmpty items
      feed =
        (F.nullFeed siteUrl (F.TextString $ listingTitle lr <> " - r/TheMotte") lastUpdated)
          { F.feedEntries = items,
            F.feedLinks = one $ (F.nullLink $ listingUrl rp lr) {F.linkRel = Just (Left "self")}
          }
   in feed

instance Default Model where
  def = Model mempty mempty mempty mempty

instance IsRoute AppRoute where
  type RouteModel AppRoute = Model

  -- TODO: Use generics
  routeEncoder = mkRouteEncoder $ \_model ->
    prism' encodeRoute decodeRoute
    where
      encodeRoute = \case
        AppRoute_Html r -> case r of
          R_Index -> "index.html"
          R_Users -> "u.html"
          R_Listing lr ->
            listingRouteBaseName lr <> ".html"
        AppRoute_Atom lr ->
          listingRouteBaseName lr <> ".atom"
        where
          listingRouteBaseName = \case
            LR_Timeline -> "timeline"
            LR_MotteSticky ms -> toString $ T.toLower (motteStickyName ms)
            LR_User name -> "u/" <> toString name
      decodeRoute = \case
        "index.html" -> Just $ AppRoute_Html R_Index
        "u.html" -> Just $ AppRoute_Html R_Users
        fp ->
          (AppRoute_Html . R_Listing <$> parseListingRoutePath ".html" fp)
            <|> (AppRoute_Atom <$> parseListingRoutePath ".atom" fp)
        where
          parseListingRoutePath ext = \case
            (T.stripSuffix ext . toText -> Just baseName) ->
              case T.stripPrefix "u/" baseName of
                Just userName ->
                  Just $ LR_User userName
                Nothing ->
                  case baseName of
                    "timeline" ->
                      Just LR_Timeline
                    _ ->
                      LR_MotteSticky <$> readMotteSticky baseName
            _ ->
              Nothing
  allRoutes model =
    let allListingRoutes =
          mconcat
            [ [LR_Timeline],
              LR_MotteSticky <$> [minBound .. maxBound],
              LR_User <$> (modelGetUsers model <&> fst)
            ]
     in mconcat
          [ [AppRoute_Html R_Index],
            [AppRoute_Html R_Users],
            AppRoute_Html . R_Listing <$> allListingRoutes,
            AppRoute_Atom <$> allListingRoutes
          ]

instance EmaSite AppRoute where
  siteInput _ _ = do
    let pats = [((), "*.json")]
        ignorePats = [".*"]
        model0 = Model mempty mempty mempty mempty
    Dynamic <$> UM.mount "content" pats ignorePats model0 (const updateHandler)
    where
      updateHandler fp action = do
        -- Consume foo-sanitizied.json
        case T.stripSuffix "-sanitizied.json" (toText fp) >>= readMotteSticky of
          Just ms ->
            case action of
              UM.Refresh _ _ -> do
                log $ "Reading " <> toText fp
                liftIO (eitherDecodeFileStrict @[Post] $ "content" </> fp) >>= \case
                  Left err -> error $ show err
                  Right posts -> do
                    log $ "Setting " <> show ms <> " to " <> show (length posts) <> " posts"
                    pure $ modelSetPosts ms posts
              UM.Delete ->
                pure id
          Nothing ->
            pure id
  siteOutput rp model r =
    render rp model r

main :: IO ()
main = Ema.runSite_ @AppRoute ()

extraHead :: Prism' FilePath AppRoute -> Route -> H.Html
extraHead rp r = do
  H.base ! A.href "/"
  let feedR = associatedListingRoute r
  H.link ! A.rel "alternate"
    ! A.type_ "application/atom+xml"
    ! A.title (H.toValue $ listingTitle feedR <> "- r/TheMotte")
    ! A.href (H.toValue $ routeUrl rp $ AppRoute_Atom feedR)
  -- TODO: until we get windicss compilation
  H.style
    " .extlink:visited { \
    \   color: dimgray; \
    \ } \
    \ .extlink:link { \
    \  font-weight: 600; \
    \ } "
  where
    -- Used to hint at the feed for current route.
    associatedListingRoute = \case
      R_Listing lr ->
        lr
      _ ->
        LR_Timeline

headTitle :: Route -> H.Html
headTitle r = do
  let siteTitle = "r/TheMotte dashboard"
  case r of
    R_Index ->
      H.title $ H.toHtml siteTitle
    R_Users ->
      H.title $ H.toHtml $ "Users - " <> siteTitle
    R_Listing lr ->
      H.title $ H.toHtml $ listingTitle lr <> " - " <> siteTitle

data ViewMode
  = ViewGrid
  | ViewFull
  deriving (Eq, Show)

render :: Prism' FilePath AppRoute -> Model -> AppRoute -> Ema.Asset LByteString
render rp model = \case
  AppRoute_Html htmlRoute ->
    Ema.AssetGenerated Ema.Html $ renderHtml rp model htmlRoute
  AppRoute_Atom lr ->
    let feed = listingFeed rp model lr
     in Ema.AssetGenerated Ema.Other $ encodeUtf8 $ fromMaybe (error "Feed malformed?") $ F.textFeed feed

renderHtml :: Prism' FilePath AppRoute -> Model -> Route -> LByteString
renderHtml rp model r = do
  let now = unsafePerformIO getCurrentTime
  tailwindLayout (headTitle r >> extraHead rp r) $
    H.main ! A.class_ "mx-auto" $ do
      H.div ! A.class_ "my-2 p-4" $ do
        H.div $ do
          H.div ! A.class_ "flex items-center justify-center gap-4" $ do
            let linkBg tr = if tr == r || (tr == R_Users && isUserRoute r) then "bg-blue-300" else ""
                mkLink tr = H.a ! A.class_ (linkBg tr <> " px-1 py-0.5 rounded") ! routeHref (AppRoute_Html tr)
            mkLink (R_Listing LR_Timeline) "Timeline View"
            mkLink R_Index "Dashboard View"
            mkLink R_Users "User View"
        case r of
          R_Index -> do
            H.div ! A.class_ "flex flex-wrap items-stretch" $ do
              forM_ [minBound .. maxBound] $ \ms ->
                H.div ! A.class_ "w-full md:w-1/2 lg:w-1/3 xl:w-1/4 2xl:w-1/5 overflow-hidden flex-grow" $ do
                  H.div ! A.class_ ("bg-" <> sectionClr ms <> "-50 my-2 mx-2 p-2") $
                    renderSection ms ViewGrid
          R_Listing LR_Timeline -> do
            H.div ! A.class_ "my-2 p-2 container mx-auto" $
              renderTimeline $ modelGetPostTimeline model
          R_Listing (LR_MotteSticky ms) -> do
            H.div ! A.class_ ("bg-" <> sectionClr ms <> "-50 my-2 p-2 container mx-auto") $
              renderSection ms ViewFull
          R_Users -> do
            H.div ! A.class_ "my-2 p-2 container mx-auto" $ do
              H.div ! A.class_ "flex flex-col gap-2" $ do
                forM_ (modelGetUsers model & sortOn (Down . length . snd)) $ \(user, posts) -> do
                  H.div ! A.class_ "flex flex-nowrap items-center gap-2" $ do
                    renderPostAuthor user
                    H.span ! A.class_ "text-gray-500 text-xs font-mono" $ H.toHtml $ length posts
          R_Listing (LR_User name) -> do
            H.div ! A.class_ "my-2 p-2 container mx-auto" $ do
              renderRouteHeading R_Users "blue" $ H.toHtml $ "u/" <> name
              renderTimeline $ modelGetUserPosts model name
        H.div ! A.class_ "flex items-center justify-center" $ do
          H.div ! A.class_ "text-sm text-gray-400" $ do
            "Generated on "
            renderTime now
        H.div ! A.class_ "flex items-center justify-center" $ do
          H.div ! A.class_ "text-sm text-gray-500" $ do
            H.p $ do
              "Powered by "
              H.a ! A.class_ "font-semibold" ! A.href "https://ema.srid.ca" $ "Ema"
              " | "
              H.a ! A.href "https://github.com/EmaApps/TheMotteDashboard" $ "Source & Feedback"
  where
    renderTime t = do
      H.span ! A.title (H.toValue $ show @Text t) $ H.toHtml $ showTime t
    sectionClr = \case
      MS_CultureWar -> "red"
      MS_WellnessWednesday -> "green"
      MS_SmallScaleQuestions -> "gray"
      MS_FridayFun -> "purple"
    renderSection motteSticky viewMode = do
      let otherRoute = case viewMode of
            ViewGrid -> R_Listing $ LR_MotteSticky motteSticky
            ViewFull -> R_Index
      H.h1 ! A.class_ ("py-1 text-2xl italic font-semibold font-mono border-b-2 bg-" <> sectionClr motteSticky <> "-200") $ do
        H.a ! routeHref (AppRoute_Html otherRoute) ! A.title "Switch View" ! A.class_ "flex items-center justify-center" $ do
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
        H.a ! routeHref (AppRoute_Html $ R_Listing $ LR_User author) $
          "u/" <> H.toHtml author
    renderRouteHeading headingR clr w =
      H.h1 ! A.class_ ("py-1 text-2xl italic font-semibold font-mono border-b-2 bg-" <> clr <> "-200") $ do
        H.a ! routeHref (AppRoute_Html headingR) ! A.title "Switch View" ! A.class_ "flex items-center justify-center" $ do
          w

    renderPostBody motteSticky viewMode p@Post {..} =
      H.blockquote ! A.class_ ("ml-2 pl-2 border-l-2 overflow-hidden hover:border-" <> sectionClr motteSticky <> "-600") $ do
        let n = 80
            nn = if viewMode == ViewGrid then 200 else 700
        -- TODO: After moving to windicss, replace extlink with visited:text-gray-500
        H.a ! postLinkAttr p ! A.class_ "extlink" $ H.toHtml $ T.take n postBody
        H.a ! postLinkAttr p ! A.class_ "text-gray-500" $ do
          H.toHtml $ T.take nn $ T.drop n postBody
          "..."

    renderPostTime p =
      H.a ! A.class_ "text-xs text-gray-400" ! postLinkAttr p $ do
        renderTime $ postTime p

    renderTimeline :: [(MotteSticky, Post)] -> H.Html
    renderTimeline posts =
      forM_ posts $ \(ms, post@Post {..}) -> do
        H.div ! A.class_ ("p-2 flex flex-col md:flex-row  bg-" <> sectionClr ms <> "-50") ! A.title (H.toValue $ motteStickyLongName ms) $ do
          H.div ! A.class_ "font-mono flex flex-col w-32" $ do
            renderPostTime post
            H.div ! A.class_ "mt-0.5 text-xs overflow-hidden text-gray-600" $ renderPostAuthor postAuthor
          H.div ! A.class_ "flex-1" $ renderPostBody ms ViewFull post

    postLinkAttr post =
      mconcat [A.target "blank", A.href (H.toValue $ postUrl post)]

    routeHref r' =
      A.href (fromString . toString $ routeUrl rp r')

showTime :: UTCTime -> Text
showTime =
  toText . formatTime defaultTimeLocale "%b %d, %R UTC"

-- | A simple and off-the-shelf layout using Tailwind CSS
tailwindLayout :: H.Html -> H.Html -> LByteString
tailwindLayout h b =
  layoutWith "en" "UTF-8" (tailwind2ShimCdn >> h) $
    -- The "overflow-y-scroll" makes the scrollbar visible always, so as to
    -- avoid janky shifts when switching to routes with suddenly scrollable content.
    H.body ! A.class_ "overflow-y-scroll" $ b
  where
    -- A general layout
    layoutWith :: H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html -> LByteString
    layoutWith lang encoding appHead appBody = RU.renderHtml $ do
      H.docType
      H.html ! A.lang lang $ do
        H.head $ do
          H.meta ! A.charset encoding
          -- This makes the site mobile friendly by default.
          H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
          appHead
        appBody

    -- Loads full tailwind CSS from CDN (not good for production)
    tailwind2ShimCdn :: H.Html
    tailwind2ShimCdn =
      H.link
        ! A.href "https://unpkg.com/tailwindcss@2/dist/tailwind.min.css"
        ! A.rel "stylesheet"
        ! A.type_ "text/css"

log :: MonadLogger m => Text -> m ()
log = logInfoNS "TheMotteDashboard"

logD :: MonadLogger m => Text -> m ()
logD = logDebugNS "TheMotteDashboard"
