module Main where

import qualified Configuration.Dotenv as Dotenv
import System.Environment (lookupEnv)

main :: IO ()
main = do
  -- Load environment variables from .env file
  Dotenv.loadFile Dotenv.defaultConfig

  -- Get the DISCORD_TOKEN from the environment
  maybeToken <- lookupEnv "DISCORD_TOKEN"
  case maybeToken of
    Just token -> putStrLn $ "Token loaded: " ++ token
    Nothing -> putStrLn "Error: DISCORD_TOKEN not found in .env file"

  putStrLn "Hello, :wink:!"