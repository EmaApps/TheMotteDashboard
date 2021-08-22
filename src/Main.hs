{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Logger
import Data.Aeson (FromJSON (parseJSON), eitherDecodeFileStrict)
import Data.Aeson.Options (genericParseJSONStripType)
import Data.Default (Default (..))
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
  | About
  deriving (Show, Enum, Bounded)

data CWTop = CWTop
  { cWTopKind :: Text,
    cWTopId :: Text,
    cWTopAuthor :: Text,
    cWTopCreatedUtc :: Int,
    cWTopBody :: Text
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
      About -> "about.html"
  decodeRoute _model = \case
    "index.html" -> Just Index
    "about.html" -> Just About
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
render emaAction model r =
  Tailwind.layout emaAction (H.title "Basic site" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "mt-8 p-2" $ do
        case r of
          Index -> do
            "You are on the index page. "
            routeElem About "Go to About"
            H.pre $ H.toHtml (shower model)
          About -> do
            "You are on the about page. "
            routeElem Index "Go to Index"
  where
    routeElem r' w =
      H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl model r')
