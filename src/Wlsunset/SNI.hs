{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Standalone StatusNotifierItem service for controlling @wlsunset@.
module Wlsunset.SNI
  ( Config (..),
    defaultConfig,
    runWlsunsetSNI,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch)
import Control.Monad (forever, void, when)
import DBus
import DBus.Client
  ( Client,
    Interface (..),
    RequestNameReply (..),
    autoMethod,
    connectSession,
    defaultInterface,
    export,
    readOnlyProperty,
    requestName,
  )
import DBus.Proxy (proxyAll)
import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.String (fromString)
import qualified Data.Text as T
import qualified GI.Dbusmenu as Dbusmenu
import qualified GI.GLib as GLib
import qualified GI.Gio as Gio
import qualified StatusNotifier.Item.Client as I
import qualified StatusNotifier.Watcher.Client as W
import System.Exit (exitSuccess)
import System.Log.Logger
  ( Priority (..),
    getRootLogger,
    saveGlobalLogger,
    setLevel,
  )
import Wlsunset.Menu
import Wlsunset.Process

-- | Runtime configuration.
data Config = Config
  { configDBusName :: String,
    configWlsunsetCommand :: String,
    configHighTemp :: Int,
    configLowTemp :: Int,
    configPollIntervalSec :: Int,
    configIconAuto :: String,
    configIconHigh :: String,
    configIconLow :: String,
    configIconOff :: String,
    configTitle :: String,
    configLogLevel :: Priority
  }

-- | Default runtime configuration.
defaultConfig :: Config
defaultConfig =
  Config
    { configDBusName = "org.taffybar.WlsunsetSNI",
      configWlsunsetCommand = "wlsunset",
      configHighTemp = 6500,
      configLowTemp = 4000,
      configPollIntervalSec = 2,
      configIconAuto = "video-display",
      configIconHigh = "video-display",
      configIconLow = "weather-clear-night",
      configIconOff = "process-stop",
      configTitle = "wlsunset",
      configLogLevel = WARNING
    }

-- | Run the SNI process, exporting item + menu services onto DBus and blocking
-- forever.
runWlsunsetSNI :: Config -> IO ()
runWlsunsetSNI cfg@Config {..} = do
  setupLogging configLogLevel

  let busName = configDBusName
      path = "/StatusNotifierItem"
      menuPath = path <> "/Menu"
      menuBusName = busName <> ".Menu"
      iconObjectPath = objectPath_ path

  client <- connectSession

  -- Menu is hosted via gi-dbusmenu on a separate bus name, then proxied onto
  -- the item bus name (see DBus.Proxy).
  menuConnection <- Just <$> Gio.cancellableNew >>= Gio.busGetSync Gio.BusTypeSession
  void $ Gio.busOwnNameOnConnection menuConnection (T.pack menuBusName) [] Nothing Nothing
  menuServer <- Dbusmenu.serverNew (T.pack menuPath)

  -- GLib main loop needed by gi-dbusmenu.
  mainLoop <- GLib.mainLoopNew Nothing False >>= GLib.mainLoopRef
  glibContext <- GLib.mainLoopGetContext mainLoop
  void $ forkIO $ GLib.mainLoopRun mainLoop

  let wsCfg =
        WlsunsetConfig
          { wlsunsetCommand = configWlsunsetCommand,
            wlsunsetHighTemp = configHighTemp,
            wlsunsetLowTemp = configLowTemp,
            wlsunsetPollIntervalSec = configPollIntervalSec
          }
  wlsunset <- newWlsunset wsCfg

  let rebuildMenu :: IO ()
      rebuildMenu = do
        st <- getWlsunsetState wlsunset
        let actions =
              MenuActions
                { onSetMode = \mode -> do
                    st0 <- getWlsunsetState wlsunset
                    when (not (wlsunsetRunning st0)) (startWlsunset wlsunset)
                    cycleWlsunsetToMode wlsunset mode,
                  onToggle = toggleWlsunset wlsunset,
                  onSetFixedTemp = \t -> restartWlsunsetWithTemps wlsunset t t,
                  onResetTemps = restartWlsunsetWithTemps wlsunset configLowTemp configHighTemp,
                  onQuit = exitSuccess
                }
        resultVar <- newEmptyMVar
        runOnGLibMain glibContext $
          ( do
              newRoot <- buildMenu wsCfg st actions
              Dbusmenu.serverSetRoot menuServer newRoot
              putMVar resultVar (Right ())
          )
            `catch` (\(e :: SomeException) -> putMVar resultVar (Left e))
        takeMVar resultVar >>= \case
          Left e -> fail ("Failed to rebuild menu: " <> show e)
          Right () -> pure ()

  -- Initial menu
  rebuildMenu

  exportSNI cfg client path menuPath wlsunset
  nameReply <- requestName client (busName_ busName) []
  when
    (nameReply /= NamePrimaryOwner && nameReply /= NameAlreadyOwner)
    (fail ("Failed to acquire DBus name " <> busName <> ": " <> show nameReply))

  -- Proxy the menu from menuBusName onto busName at the same object path, so
  -- hosts can find com.canonical.dbusmenu at busName:/StatusNotifierItem/Menu.
  proxyAll client (busName_ menuBusName) (objectPath_ menuPath) (objectPath_ menuPath)

  -- Register with the watcher (must be done from the same DBus connection that
  -- owns the SNI bus name).
  W.registerStatusNotifierItem client busName >>= \case
    Left methodErr ->
      fail ("Failed to register StatusNotifierItem: " <> show methodErr)
    Right _ ->
      pure ()

  -- React to state changes.
  chan <- dupWlsunsetChan wlsunset
  void $ forkIO $ forever $ do
    _st <- atomically $ readTChan chan
    rebuildMenu
    emitSafe $ I.emitNewIcon client iconObjectPath
    emitSafe $ I.emitNewToolTip client iconObjectPath

  forever $ threadDelay maxBound

exportSNI :: Config -> Client -> String -> String -> Wlsunset -> IO ()
exportSNI cfg client path menuPath wlsunset = do
  let iface =
        defaultInterface
          { interfaceName = interfaceName_ "org.kde.StatusNotifierItem",
            interfaceMethods =
              [ autoMethod "SecondaryActivate" (\(_x :: Int32) (_y :: Int32) -> toggleWlsunset wlsunset),
                autoMethod "Activate" (\(_x :: Int32) (_y :: Int32) -> (return () :: IO ())),
                autoMethod "ContextMenu" (\(_x :: Int32) (_y :: Int32) -> (return () :: IO ())),
                autoMethod "Scroll" (\(_delta :: Int32) (_orientation :: String) -> (return () :: IO ()))
              ],
            interfaceProperties =
              [ readOnlyProperty "Category" (pure ("ApplicationStatus" :: String)),
                readOnlyProperty "Id" (pure ("wlsunset-sni" :: String)),
                readOnlyProperty "Title" (pure (configTitle cfg)),
                readOnlyProperty "Status" (pure ("Active" :: String)),
                readOnlyProperty "WindowId" (pure (0 :: Int32)),
                readOnlyProperty "IconThemePath" (pure ("" :: String)),
                readOnlyProperty "IconName" (stateIconName cfg <$> getWlsunsetState wlsunset),
                readOnlyProperty "OverlayIconName" (pure ("" :: String)),
                readOnlyProperty "ItemIsMenu" (pure True),
                readOnlyProperty "Menu" (pure (objectPath_ menuPath)),
                readOnlyProperty "ToolTip" (getWlsunsetState wlsunset >>= tooltip cfg)
              ],
            interfaceSignals = []
          }
  export client (fromString path) iface

stateIconName :: Config -> WlsunsetState -> String
stateIconName Config {..} st
  | not (wlsunsetRunning st) = configIconOff
  | otherwise =
      case wlsunsetMode st of
        WlsunsetAuto -> configIconAuto
        WlsunsetForcedHighTemp -> configIconHigh
        WlsunsetForcedLowTemp -> configIconLow

-- org.kde.StatusNotifierItem.ToolTip is (s a(iiay) s s)
-- We don't provide pixmaps; most hosts are fine with IconName + text.
tooltip :: Config -> WlsunsetState -> IO (String, [(Int32, Int32, BS.ByteString)], String, String)
tooltip cfg st =
  let title = configTitle cfg
      text = T.unpack (tooltipText st)
   in pure ("", [], title, text)

tooltipText :: WlsunsetState -> T.Text
tooltipText st =
  let highT = T.pack (show (wlsunsetEffectiveHighTemp st) <> "K")
      lowT = T.pack (show (wlsunsetEffectiveLowTemp st) <> "K")
   in case (wlsunsetRunning st, wlsunsetMode st) of
        (False, _) -> "wlsunset: stopped"
        (True, WlsunsetAuto) -> "wlsunset: automatic (" <> lowT <> " - " <> highT <> ")"
        (True, WlsunsetForcedHighTemp) -> "wlsunset: high temp (" <> highT <> ")"
        (True, WlsunsetForcedLowTemp) -> "wlsunset: low temp (" <> lowT <> ")"

setupLogging :: Priority -> IO ()
setupLogging level = do
  logger <- getRootLogger
  saveGlobalLogger (setLevel level logger)

emitSafe :: IO () -> IO ()
emitSafe io = io `catch` (\(_ :: SomeException) -> pure ())

-- Schedule work on the GLib main context.
runOnGLibMain :: GLib.MainContext -> IO () -> IO ()
runOnGLibMain context action = do
  void $
    GLib.mainContextInvokeFull
      (Just context)
      GLib.PRIORITY_DEFAULT
      (action >> pure False)
