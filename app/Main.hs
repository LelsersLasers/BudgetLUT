-- Allows "strings" to be Data.Text
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Configuration.Dotenv as Dotenv
import Control.Monad (unless, void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import System.Environment (lookupEnv)
import UnliftIO (liftIO)
import UnliftIO.Concurrent

-- Constants for help and error messages
helpMessage :: T.Text
helpMessage = "Use `!lut help` for more specifics about the !lut command"

lutHelpMessage :: T.Text
lutHelpMessage = "TODO!"

lutUnknownCommand :: T.Text
lutUnknownCommand = "Unknown command. Use `!lut help` for all the available actions."

lutAddNoName :: T.Text
lutAddNoName = "You need to provide a name for the lut. Use !lut add <name of lut>"

lutAddNoAttachment :: T.Text
lutAddNoAttachment = "You need to provide exactly one attachment for the lut."

-- Main function
main :: IO ()
main = do
  -- Load environment variables from .env file
  Dotenv.loadFile Dotenv.defaultConfig

  -- Get the DISCORD_TOKEN from the environment
  maybeToken <- lookupEnv "DISCORD_TOKEN"
  tok <- case maybeToken of
    Just token -> return (T.pack token)
    Nothing -> fail "DISCORD_TOKEN not found in environment"

  -- Run the Discord bot
  err <-
    runDiscord $
      def
        { discordToken = tok,
          discordOnEnd = liftIO $ threadDelay (round (0.4 :: Double) * (10 ^ (6 :: Int))) >> putStrLn "\nDone!",
          discordOnEvent = eventHandler,
          discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn T.empty,
          discordGatewayIntent = def {gatewayIntentMessageContent = True}
        }
  TIO.putStrLn err

-- Event handler
eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> unless (fromBot m) $ handleMessage m
  _ -> return ()

-- Handle incoming messages
handleMessage :: Message -> DiscordHandler ()
handleMessage m
  | isHelp m = sendHelpMessage m
  | isLut m = handleLutCommand m
  | otherwise = return ()

-- Send help message
sendHelpMessage :: Message -> DiscordHandler ()
sendHelpMessage m = sendMessage m helpMessage

-- Handle !lut commands
handleLutCommand :: Message -> DiscordHandler ()
handleLutCommand m = do
  let parts = tail $ T.words $ messageContent m
  case parts of
    ["help"] -> sendMessage m lutHelpMessage
    ["add"] -> sendMessage m lutAddNoName
    "add" : nameParts -> handleLutAdd m nameParts
    _ -> sendMessage m lutUnknownCommand

-- Handle !lut add command
handleLutAdd :: Message -> [T.Text] -> DiscordHandler ()
handleLutAdd m nameParts = do
  let attachments = messageAttachments m
  case attachments of
    [a] -> do
      let name = T.unwords nameParts
      sendMessage m $ "Adding " <> attachmentUrl a <> " as " <> name
    _ -> sendMessage m lutAddNoAttachment

-- Helper function to send a message with a reference to the original message
sendMessage :: Message -> T.Text -> DiscordHandler ()
sendMessage m content = do
  let opts =
        def
          { R.messageDetailedContent = content,
            R.messageDetailedReference = Just $ def {referenceMessageId = Just $ messageId m}
          }
  void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts)

-- Check if a message is from a bot
fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

-- Check if a message is a !help command
isHelp :: Message -> Bool
isHelp = ("!help" `T.isPrefixOf`) . T.toLower . messageContent

-- Check if a message is a !lut command
isLut :: Message -> Bool
isLut = ("!lut" `T.isPrefixOf`) . T.toLower . messageContent