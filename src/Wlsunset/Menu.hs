{-# LANGUAGE OverloadedStrings #-}

-- | DBusMenu construction for the @wlsunset-sni@ tray item.
module Wlsunset.Menu
  ( MenuActions (..),
    buildMenu,
  )
where

import Control.Concurrent (forkIO)
import Control.Monad (forM_, void)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import GI.Dbusmenu
import Wlsunset.Process

-- | Actions invoked by menu items.
data MenuActions = MenuActions
  { onSetMode :: WlsunsetMode -> IO (),
    onToggle :: IO (),
    onSetFixedTemp :: Int -> IO (),
    onResetTemps :: IO (),
    onQuit :: IO ()
  }

-- | Temperature presets from 2500K to 6500K in 500K increments.
tempPresets :: [Int]
tempPresets = [2500, 3000, 3500, 4000, 4500, 5000, 5500, 6000, 6500]

-- | Build a full DBusMenu tree for the current @wlsunset@ state.
buildMenu :: WlsunsetConfig -> WlsunsetState -> MenuActions -> IO Menuitem
buildMenu cfg st actions = do
  root <- menuitemNew

  header <- makeLabel (stateLine cfg st)
  setEnabled header False
  void $ menuitemChildAppend root header

  addSeparator root

  -- Mode items (disabled when not running)
  let running = wlsunsetRunning st
      currentMode = wlsunsetMode st
      effectiveHigh = wlsunsetEffectiveHighTemp st
      effectiveLow = wlsunsetEffectiveLowTemp st
      highT = T.pack (show effectiveHigh <> "K")
      lowT = T.pack (show effectiveLow <> "K")
      modes :: [(Text, WlsunsetMode)]
      modes =
        [ ("Automatic", WlsunsetAuto),
          ("High Temp (" <> highT <> ")", WlsunsetForcedHighTemp),
          ("Low Temp (" <> lowT <> ")", WlsunsetForcedLowTemp)
        ]

  forM_ modes $ \(labelText, targetMode) -> do
    item <- makeLabel labelText
    setToggleType item "radio"
    setToggleState item (if running && currentMode == targetMode then 1 else 0)
    setEnabled item running
    void $ onMenuitemItemActivated item $ \_ ->
      void $ forkIO $ onSetMode actions targetMode
    void $ menuitemChildAppend root item

  addSeparator root

  -- Temperature presets submenu
  tempParent <- makeLabel "Set Temperature"
  void $ menuitemPropertySet tempParent "children-display" "submenu"

  let isFixedTemp = effectiveLow == effectiveHigh
  forM_ tempPresets $ \temp -> do
    tempItem <- makeLabel (T.pack (show temp <> "K"))
    setToggleType tempItem "radio"
    setToggleState tempItem (if running && isFixedTemp && effectiveLow == temp then 1 else 0)
    setEnabled tempItem running
    void $ onMenuitemItemActivated tempItem $ \_ ->
      void $ forkIO $ onSetFixedTemp actions temp
    void $ menuitemChildAppend tempParent tempItem

  addSeparator tempParent

  let defaultLow = wlsunsetLowTemp cfg
      defaultHigh = wlsunsetHighTemp cfg
      resetLabel =
        T.pack $
          "Reset (" <> show defaultLow <> "K  -  " <> show defaultHigh <> "K)"
  resetItem <- makeLabel resetLabel
  setEnabled resetItem running
  setToggleType resetItem "radio"
  setToggleState resetItem (if running && effectiveLow == defaultLow && effectiveHigh == defaultHigh then 1 else 0)
  void $ onMenuitemItemActivated resetItem $ \_ ->
    void $ forkIO $ onResetTemps actions
  void $ menuitemChildAppend tempParent resetItem

  void $ menuitemChildAppend root tempParent

  addSeparator root

  toggleItem <- makeLabel (if running then "Stop wlsunset" else "Start wlsunset")
  void $ onMenuitemItemActivated toggleItem $ \_ ->
    void $ forkIO $ onToggle actions
  void $ menuitemChildAppend root toggleItem

  quitItem <- makeLabel "Quit"
  void $ onMenuitemItemActivated quitItem $ \_ ->
    void $ forkIO $ onQuit actions
  void $ menuitemChildAppend root quitItem

  pure root

stateLine :: WlsunsetConfig -> WlsunsetState -> Text
stateLine _cfg st =
  let highT = T.pack (show (wlsunsetEffectiveHighTemp st) <> "K")
      lowT = T.pack (show (wlsunsetEffectiveLowTemp st) <> "K")
   in case (wlsunsetRunning st, wlsunsetMode st) of
        (False, _) -> "wlsunset: stopped"
        (True, WlsunsetAuto) -> "wlsunset: automatic (" <> lowT <> "  -  " <> highT <> ")"
        (True, WlsunsetForcedHighTemp) -> "wlsunset: high temp (" <> highT <> ")"
        (True, WlsunsetForcedLowTemp) -> "wlsunset: low temp (" <> lowT <> ")"

-- Helpers

makeLabel :: Text -> IO Menuitem
makeLabel text = do
  item <- menuitemNew
  void $ menuitemPropertySet item "label" text
  pure item

addSeparator :: Menuitem -> IO ()
addSeparator parent = do
  sep <- menuitemNew
  void $ menuitemPropertySet sep "type" ("separator" :: Text)
  void $ menuitemChildAppend parent sep

setEnabled :: Menuitem -> Bool -> IO ()
setEnabled item enabled =
  void $ menuitemPropertySetBool item "enabled" enabled

setToggleType :: Menuitem -> Text -> IO ()
setToggleType item toggleType =
  void $ menuitemPropertySet item "toggle-type" toggleType

setToggleState :: Menuitem -> Int32 -> IO ()
setToggleState item toggleState =
  void $ menuitemPropertySetInt item "toggle-state" toggleState
