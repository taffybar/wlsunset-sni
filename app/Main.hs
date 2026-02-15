module Main where

import Options.Applicative
import System.Log.Logger (Priority (..))
import Wlsunset.SNI

data Opts = Opts
  { optDBusName :: String
  , optCommand :: String
  , optHighTemp :: Int
  , optLowTemp :: Int
  , optPollInterval :: Int
  , optLogLevel :: Priority
  }

optsParser :: Parser Opts
optsParser =
  Opts
    <$> strOption
      ( long "dbus-name"
          <> short 'b'
          <> metavar "BUS-NAME"
          <> value "org.taffybar.WlsunsetSNI"
      )
    <*> strOption
      ( long "command"
          <> short 'c'
          <> metavar "CMD"
          <> value "wlsunset"
          <> help "Full command used to start wlsunset (shell string)."
      )
    <*> option auto
      ( long "high-temp"
          <> metavar "K"
          <> value 6500
          <> help "High (day) temperature used for display and reset preset."
      )
    <*> option auto
      ( long "low-temp"
          <> metavar "K"
          <> value 4000
          <> help "Low (night) temperature used for display and reset preset."
      )
    <*> option auto
      ( long "poll-interval"
          <> metavar "SECONDS"
          <> value 2
          <> help "Polling interval for wlsunset process status."
      )
    <*> option auto
      ( long "log-level"
          <> metavar "LEVEL"
          <> value WARNING
          <> help "hslogger Priority (DEBUG|INFO|NOTICE|WARNING|ERROR|CRITICAL|ALERT|EMERGENCY)."
      )

main :: IO ()
main = do
  Opts { optDBusName = busName
       , optCommand = cmd
       , optHighTemp = highT
       , optLowTemp = lowT
       , optPollInterval = pollSec
       , optLogLevel = level
       } <- execParser $
    info (helper <*> optsParser)
      ( fullDesc
          <> progDesc "Run a StatusNotifierItem tray icon for controlling wlsunset"
      )

  let cfg =
        defaultConfig
          { configDBusName = busName
          , configWlsunsetCommand = cmd
          , configHighTemp = highT
          , configLowTemp = lowT
          , configPollIntervalSec = pollSec
          , configLogLevel = level
          }

  runWlsunsetSNI cfg
