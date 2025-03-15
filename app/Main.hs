-- Allows "strings" to be Data.Text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Codec.Picture (convertRGBA8, decodeImage, encodePng)
import qualified Configuration.Dotenv as Dotenv
import Control.Exception (SomeException, try)
import Control.Monad (replicateM, unless, void)
import Data.Acid
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import KeyValueStore
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.Random
import UnliftIO (liftIO)
import UnliftIO.Concurrent

-- Constants for file saving
lutFolder :: FilePath
lutFolder = "luts"

acidPath :: FilePath
acidPath = lutFolder <> "/acid"

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

  acid <- openLocalStateFrom acidPath emptyStore

  -- Run the Discord bot
  err <-
    runDiscord $
      def
        { discordToken = tok,
          discordOnEnd = liftIO $ threadDelay (round (0.4 :: Double) * (10 ^ (6 :: Int))) >> putStrLn "\nDone!",
          discordOnEvent = eventHandler acid,
          discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn T.empty,
          discordGatewayIntent = def {gatewayIntentMessageContent = True}
        }
  TIO.putStrLn err

-- Event handler
eventHandler :: AcidState KeyValueStore -> Event -> DiscordHandler ()
eventHandler acid event = case event of
  MessageCreate m -> unless (fromBot m) $ handleMessage acid m
  _ -> return ()

-- Handle incoming messages
handleMessage :: AcidState KeyValueStore -> Message -> DiscordHandler ()
handleMessage acid m
  | isHelp m = sendHelpMessage m
  | isLut m = handleLutCommand acid m
  | otherwise = return ()

-- Send help message
sendHelpMessage :: Message -> DiscordHandler ()
sendHelpMessage m = sendMessage m helpMessage

-- Handle !lut commands
handleLutCommand :: AcidState KeyValueStore -> Message -> DiscordHandler ()
handleLutCommand acid m = do
  let parts = tail $ T.words $ messageContent m
  case parts of
    ["help"] -> sendMessage m lutHelpMessage
    ["add"] -> sendMessage m lutAddNoName
    "add" : nameParts -> handleLutAdd acid m nameParts
    _ -> sendMessage m lutUnknownCommand

-- Handle !lut add command
handleLutAdd :: AcidState KeyValueStore -> Message -> [T.Text] -> DiscordHandler ()
handleLutAdd acid m nameParts = do
  let attachments = messageAttachments m
  case attachments of
    [a] -> do
      let name = T.unwords nameParts
      code <- generateUniqueCode acid
      liftIO $ update acid (InsertKeyValue code name)
      let url = attachmentUrl a
      let filename = lutFolder <> "/" <> T.unpack code <> ".png"
      success <- liftIO $ downloadFile (T.unpack url) filename
      if success
        then sendMessage m $ "Added LUT: *" <> name <> "* as **" <> code <> "**"
        else do
          _ <- liftIO $ update acid (RemoveKeyValue code)
          sendMessage m "Failed to download the file. Make sure you upload a valid image!"
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

-- Download a file from a URL and save it to a local file
downloadFile :: String -> FilePath -> IO Bool
downloadFile url filename = do
  createDirectoryIfMissing True (takeDirectory filename)
  result <- try $ do
    request <- parseRequest url
    response <- httpBS request
    let content = getResponseBody response -- content :: BS.ByteString
    case decodeImage content of
      Left _err -> return False -- Decoding failed
      Right dynamicImage -> do
        let image = convertRGBA8 dynamicImage -- Convert to Image PixelRGBA8
        BL.writeFile filename (encodePng image)
        return True -- Success
  case result of
    Left (_err :: SomeException) -> return False -- Exception occurred
    Right success -> return success -- Return the result of the operation

-- Generate a 3 long code that contains numbers or capital letters
generateCode :: IO T.Text
generateCode = do
  chars <-
    replicateM 3 $
      randomRIO ('0', 'Z') >>= \c ->
        if c `elem` (['0' .. '9'] ++ ['A' .. 'Z'])
          then return c
          else randomRIO ('0', 'Z') -- Retry if the character is not valid
  return $ T.pack chars

-- Check of a code is already used
isCodeUsed :: AcidState KeyValueStore -> T.Text -> DiscordHandler Bool
isCodeUsed acid code = do
  result <- liftIO $ query acid (LookupKeyValue code) -- Lift IO to DiscordHandler
  return $ isJust result

-- Generate a unique code
generateUniqueCode :: AcidState KeyValueStore -> DiscordHandler T.Text
generateUniqueCode acid = do
  code <- liftIO generateCode -- Lift IO to DiscordHandler
  used <- isCodeUsed acid code
  if used
    then generateUniqueCode acid -- Retry if the code is already used
    else return code

-- Check if a message is from a bot
fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

-- Check if a message is a !help command
isHelp :: Message -> Bool
isHelp = ("!help" `T.isPrefixOf`) . T.toLower . messageContent

-- Check if a message is a !lut command
isLut :: Message -> Bool
isLut = ("!lut" `T.isPrefixOf`) . T.toLower . messageContent