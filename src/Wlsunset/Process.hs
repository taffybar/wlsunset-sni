{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wlsunset.Process
  ( WlsunsetMode (..)
  , WlsunsetState (..)
  , WlsunsetConfig (..)
  , Wlsunset
  , newWlsunset
  , getWlsunsetState
  , dupWlsunsetChan
  , cycleWlsunsetMode
  , cycleWlsunsetToMode
  , startWlsunset
  , stopWlsunset
  , toggleWlsunset
  , restartWlsunsetWithTemps
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, catch)
import Control.Monad (forever, replicateM_, void, when)
import Data.List (isPrefixOf)
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
  { wlsunsetRunning :: Bool
  , wlsunsetMode :: WlsunsetMode
  , wlsunsetEffectiveHighTemp :: Int
  , wlsunsetEffectiveLowTemp :: Int
  }
  deriving (Eq, Show)

-- | Configuration for monitoring and starting wlsunset.
data WlsunsetConfig = WlsunsetConfig
  { wlsunsetCommand :: String
  , wlsunsetHighTemp :: Int
  , wlsunsetLowTemp :: Int
  , wlsunsetPollIntervalSec :: Int
  }
  deriving (Eq, Show)

-- | A running wlsunset monitor.
data Wlsunset = Wlsunset
  { wsCfg :: WlsunsetConfig
  , wsStateVar :: MVar WlsunsetState
  , wsChan :: TChan WlsunsetState
  }

newWlsunset :: WlsunsetConfig -> IO Wlsunset
newWlsunset wsCfg@WlsunsetConfig {..} = do
  let initial =
        WlsunsetState
          { wlsunsetRunning = False
          , wlsunsetMode = WlsunsetAuto
          , wlsunsetEffectiveHighTemp = wlsunsetHighTemp
          , wlsunsetEffectiveLowTemp = wlsunsetLowTemp
          }
  wsStateVar <- newMVar initial
  wsChan <- newBroadcastTChanIO
  let ws = Wlsunset {..}
  void $ forkIO $ forever $ do
    pollWlsunset ws
    threadDelay (wlsunsetPollIntervalSec * 1000000)
  pure ws

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

pgrepWlsunset :: IO [CPid]
pgrepWlsunset =
  (parsePids <$> readProcess "pgrep" ["-x", "wlsunset"] "")
    `catch` (\(_ :: SomeException) -> pure [])
  where
    parsePids = map (CPid . fromIntegral) . concatMap toList . lines
    toList s = maybe [] pure (readMaybe s :: Maybe Int)

sendUSR1 :: CPid -> IO ()
sendUSR1 = signalProcess sigUSR1

pollWlsunset :: Wlsunset -> IO ()
pollWlsunset ws = do
  pids <- pgrepWlsunset
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

cycleWlsunsetMode :: Wlsunset -> IO ()
cycleWlsunsetMode ws = do
  pids <- pgrepWlsunset
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

cycleWlsunsetToMode :: Wlsunset -> WlsunsetMode -> IO ()
cycleWlsunsetToMode ws target = do
  st <- getWlsunsetState ws
  replicateM_ (cyclesToReach (wlsunsetMode st) target) (cycleWlsunsetMode ws)

startWlsunset :: Wlsunset -> IO ()
startWlsunset ws = do
  void $ spawnCommand (wlsunsetCommand (wsCfg ws))
  broadcastUpdate ws $ \old -> old {wlsunsetRunning = True, wlsunsetMode = WlsunsetAuto}

stopWlsunset :: Wlsunset -> IO ()
stopWlsunset ws = do
  pids <- pgrepWlsunset
  mapM_ (signalProcess 15) pids
  broadcastUpdate ws $ \old -> old {wlsunsetRunning = False, wlsunsetMode = WlsunsetAuto}

toggleWlsunset :: Wlsunset -> IO ()
toggleWlsunset ws = do
  st <- getWlsunsetState ws
  if wlsunsetRunning st then stopWlsunset ws else startWlsunset ws

restartWlsunsetWithTemps :: Wlsunset -> Int -> Int -> IO ()
restartWlsunsetWithTemps ws lowTemp highTemp = do
  pids <- pgrepWlsunset
  mapM_ (signalProcess 15) pids
  let cmd = buildCommandWithTemps (wlsunsetCommand (wsCfg ws)) lowTemp highTemp
  void $ spawnCommand cmd
  broadcastUpdate ws $ \old ->
    old
      { wlsunsetRunning = True
      , wlsunsetMode = WlsunsetAuto
      , wlsunsetEffectiveHighTemp = highTemp
      , wlsunsetEffectiveLowTemp = lowTemp
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
