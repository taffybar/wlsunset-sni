{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Process-level management for @wlsunset@.
--
-- This module polls for process liveness, tracks mode transitions, and exposes
-- actions used by the SNI/menu layers.
module Wlsunset.Process
  ( WlsunsetMode (..),
    WlsunsetState (..),
    WlsunsetConfig (..),
    Wlsunset,
    newWlsunset,
    getWlsunsetState,
    dupWlsunsetChan,
    cycleWlsunsetMode,
    cycleWlsunsetToMode,
    startWlsunset,
    stopWlsunset,
    toggleWlsunset,
    restartWlsunsetWithTemps,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, catch)
import Control.Monad (forever, replicateM_, void, when)
import Data.List (isPrefixOf)
import qualified Data.List as List
import System.FilePath (takeFileName)
import System.Posix.Signals (sigUSR1, signalProcess)
import System.Posix.Types (CPid (..))
import System.Process (readProcess, spawnCommand)
import Text.Read (readMaybe)

-- | The three operating modes that wlsunset cycles through when it receives
-- SIGUSR1.
data WlsunsetMode
  = WlsunsetAuto
  | WlsunsetForcedHighTemp
  | WlsunsetForcedLowTemp
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Observable state of the wlsunset process.
data WlsunsetState = WlsunsetState
  { wlsunsetRunning :: Bool,
    wlsunsetMode :: WlsunsetMode,
    wlsunsetEffectiveHighTemp :: Int,
    wlsunsetEffectiveLowTemp :: Int
  }
  deriving (Eq, Show)

-- | Configuration for monitoring and starting wlsunset.
data WlsunsetConfig = WlsunsetConfig
  { wlsunsetCommand :: String,
    wlsunsetHighTemp :: Int,
    wlsunsetLowTemp :: Int,
    wlsunsetPollIntervalSec :: Int
  }
  deriving (Eq, Show)

-- | A running wlsunset monitor.
data Wlsunset = Wlsunset
  { wsCfg :: WlsunsetConfig,
    wsStateVar :: MVar WlsunsetState,
    wsChan :: TChan WlsunsetState
  }

-- | Create a new monitor and start a background polling loop.
newWlsunset :: WlsunsetConfig -> IO Wlsunset
newWlsunset wsCfg@WlsunsetConfig {..} = do
  let initial =
        WlsunsetState
          { wlsunsetRunning = False,
            wlsunsetMode = WlsunsetAuto,
            wlsunsetEffectiveHighTemp = wlsunsetHighTemp,
            wlsunsetEffectiveLowTemp = wlsunsetLowTemp
          }
  wsStateVar <- newMVar initial
  wsChan <- newBroadcastTChanIO
  let ws = Wlsunset {..}
  -- Ensure initial state reflects any already-running wlsunset process.
  pollWlsunset ws
  void $ forkIO $ forever $ do
    pollWlsunset ws
    threadDelay (wlsunsetPollIntervalSec * 1000000)
  pure ws

-- | Read the latest observed process state.
getWlsunsetState :: Wlsunset -> IO WlsunsetState
getWlsunsetState = readMVar . wsStateVar

-- | Create a private read-end of the broadcast channel.
dupWlsunsetChan :: Wlsunset -> IO (TChan WlsunsetState)
dupWlsunsetChan = atomically . dupTChan . wsChan

broadcastUpdate :: Wlsunset -> (WlsunsetState -> WlsunsetState) -> IO ()
broadcastUpdate ws f =
  modifyMVar_ (wsStateVar ws) $ \old -> do
    let new = f old
    when (new /= old) $ atomically $ writeTChan (wsChan ws) new
    pure new

-- ---------------------------------------------------------------------------
-- Process helpers
-- ---------------------------------------------------------------------------

pgrepWlsunset :: WlsunsetConfig -> IO [CPid]
pgrepWlsunset cfg = do
  let exactNames = candidateProcessNames cfg
  pidsFromExact <- concat <$> mapM pgrepExact exactNames
  if null pidsFromExact
    then do
      -- Fallback for wrapped command lines where argv[0] isn't the desired
      -- executable name but still contains it in the full command.
      pidsFromFuzzy <- concat <$> mapM pgrepFuzzy exactNames
      pure (dedupePids pidsFromFuzzy)
    else pure (dedupePids pidsFromExact)
  where
    pgrepExact name =
      (parsePids <$> readProcess "pgrep" ["-x", name] "")
        `catch` (\(_ :: SomeException) -> pure [])
    pgrepFuzzy name =
      (parsePids <$> readProcess "pgrep" ["-f", name] "")
        `catch` (\(_ :: SomeException) -> pure [])
    parsePids = map (CPid . fromIntegral) . concatMap toList . lines
    toList s = maybe [] pure (readMaybe s :: Maybe Int)
    dedupePids = List.nub

candidateProcessNames :: WlsunsetConfig -> [String]
candidateProcessNames cfg =
  let exeFromCommand = case words (wlsunsetCommand cfg) of
        [] -> "wlsunset"
        (exe : _) -> takeFileName exe
   in List.nub (filter (not . null) [exeFromCommand, "wlsunset"])

sendUSR1 :: CPid -> IO ()
sendUSR1 = signalProcess sigUSR1

pollWlsunset :: Wlsunset -> IO ()
pollWlsunset ws = do
  pids <- pgrepWlsunset (wsCfg ws)
  let isRunning = not (null pids)
  broadcastUpdate ws $ \old ->
    let wasRunning = wlsunsetRunning old
        newMode
          | not wasRunning && isRunning = WlsunsetAuto
          | not isRunning = WlsunsetAuto
          | otherwise = wlsunsetMode old
     in old {wlsunsetRunning = isRunning, wlsunsetMode = newMode}

-- ---------------------------------------------------------------------------
-- Actions
-- ---------------------------------------------------------------------------

-- | Send @SIGUSR1@ to all running @wlsunset@ processes and advance local mode
-- state in the Auto -> ForcedHigh -> ForcedLow -> Auto ring.
cycleWlsunsetMode :: Wlsunset -> IO ()
cycleWlsunsetMode ws = do
  pids <- pgrepWlsunset (wsCfg ws)
  case pids of
    [] -> pure ()
    _ -> do
      mapM_ sendUSR1 pids
      broadcastUpdate ws $ \old ->
        let newMode = case wlsunsetMode old of
              WlsunsetAuto -> WlsunsetForcedHighTemp
              WlsunsetForcedHighTemp -> WlsunsetForcedLowTemp
              WlsunsetForcedLowTemp -> WlsunsetAuto
         in old {wlsunsetMode = newMode, wlsunsetRunning = True}

cyclesToReach :: WlsunsetMode -> WlsunsetMode -> Int
cyclesToReach from to
  | from == to = 0
  | otherwise = (toOrd to - toOrd from) `mod` 3
  where
    toOrd WlsunsetAuto = 0
    toOrd WlsunsetForcedHighTemp = 1
    toOrd WlsunsetForcedLowTemp = 2

-- | Advance to a target mode by sending the minimum number of @SIGUSR1@
-- cycles in the standard mode ring.
cycleWlsunsetToMode :: Wlsunset -> WlsunsetMode -> IO ()
cycleWlsunsetToMode ws target = do
  st <- getWlsunsetState ws
  replicateM_ (cyclesToReach (wlsunsetMode st) target) (cycleWlsunsetMode ws)

-- | Start @wlsunset@ via the configured shell command.
startWlsunset :: Wlsunset -> IO ()
startWlsunset ws = do
  void $ spawnCommand (wlsunsetCommand (wsCfg ws))
  broadcastUpdate ws $ \old -> old {wlsunsetRunning = True, wlsunsetMode = WlsunsetAuto}

-- | Stop all running @wlsunset@ processes with @SIGTERM@.
stopWlsunset :: Wlsunset -> IO ()
stopWlsunset ws = do
  pids <- pgrepWlsunset (wsCfg ws)
  mapM_ (signalProcess 15) pids
  broadcastUpdate ws $ \old -> old {wlsunsetRunning = False, wlsunsetMode = WlsunsetAuto}

-- | Stop the process when running, otherwise start it.
toggleWlsunset :: Wlsunset -> IO ()
toggleWlsunset ws = do
  st <- getWlsunsetState ws
  if wlsunsetRunning st then stopWlsunset ws else startWlsunset ws

-- | Restart @wlsunset@ with explicit temperature bounds.
restartWlsunsetWithTemps :: Wlsunset -> Int -> Int -> IO ()
restartWlsunsetWithTemps ws lowTemp highTemp = do
  pids <- pgrepWlsunset (wsCfg ws)
  mapM_ (signalProcess 15) pids
  let cmd = buildCommandWithTemps (wlsunsetCommand (wsCfg ws)) lowTemp highTemp
  void $ spawnCommand cmd
  broadcastUpdate ws $ \old ->
    old
      { wlsunsetRunning = True,
        wlsunsetMode = WlsunsetAuto,
        wlsunsetEffectiveHighTemp = highTemp,
        wlsunsetEffectiveLowTemp = lowTemp
      }

-- ---------------------------------------------------------------------------
-- Command building
-- ---------------------------------------------------------------------------

buildCommandWithTemps :: String -> Int -> Int -> String
buildCommandWithTemps baseCmd lowTemp highTemp =
  unwords (stripTempArgs (words baseCmd))
    ++ " -t "
    ++ show lowTemp
    ++ " -T "
    ++ show highTemp

stripTempArgs :: [String] -> [String]
stripTempArgs [] = []
stripTempArgs ("-t" : _ : rest) = stripTempArgs rest
stripTempArgs ("-T" : _ : rest) = stripTempArgs rest
stripTempArgs (x : rest)
  | "-t" `isPrefixOf` x && length x > 2 = stripTempArgs rest
  | "-T" `isPrefixOf` x && length x > 2 = stripTempArgs rest
  | otherwise = x : stripTempArgs rest
