-- Allows "strings" to be Data.Text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}


module Main where

import Codec.Picture (convertRGBA8, decodeImage, encodePng, DynamicImage (ImageRGBA8), readImage, savePngImage, Image (imageWidth), imageHeight, generateImage, pixelAt, PixelRGBA8 (..), Pixel (pixelAt))
import qualified Configuration.Dotenv as Dotenv
import Control.Exception (SomeException, try)
import Control.Monad (replicateM, unless, void)
import Control.DeepSeq (NFData(..))
import Control.Parallel.Strategies
-- import Control.Parallel
import Data.Acid
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- import Data.List (nub)
import Data.List.Extra (nubOrd)
import Discord
import qualified Discord.Requests as R
import Discord.Types
import KeyValueStore
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.Random
import UnliftIO (liftIO)
import UnliftIO.Concurrent
import qualified Data.Map as Map
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import GHC.Conc (numCapabilities)
import Data.List.Split (chunksOf)

instance NFData PixelRGBA8 where
  rnf (PixelRGBA8 r g b a) = r `seq` g `seq` b `seq` a `seq` ()

-- Constants for file saving
lutFolder :: FilePath
lutFolder = "luts"

lutStorePath :: FilePath
lutStorePath = lutFolder <> "/store"

applyFolder :: FilePath
applyFolder = "apply"

applyStorePath :: FilePath
applyStorePath = applyFolder <> "/store"

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

lutRenameMissingArgs :: T.Text
lutRenameMissingArgs = "You need to provide the code and the new name for the lut. Use !lut rename <code> <new name>"

lutDeleteNoCode :: T.Text
lutDeleteNoCode = "You need to provide the code of the lut you want to delete. Use !lut delete <code>"

lutViewNoCode :: T.Text
lutViewNoCode = "You need to provide the code of the lut you want to view. Use !lut view <code>"

lutApplyNoCode :: T.Text
lutApplyNoCode = "You need to provide the code of the lut you want to apply. Use !lut apply <code>"

lutApplyNoAttachments :: T.Text
lutApplyNoAttachments = "You need to provide attachments to apply the lut."

-- Main function
main :: IO ()
main = do
  -- Use all cores but 1
  let cores = max (numCapabilities - 1) 1
  putStrLn $ "Using " <> show cores <> " cores."
  setNumCapabilities cores

  -- Load environment variables from .env file
  Dotenv.loadFile Dotenv.defaultConfig

  -- Get the DISCORD_TOKEN from the environment
  maybeToken <- lookupEnv "DISCORD_TOKEN"
  tok <- case maybeToken of
    Just token -> return (T.pack token)
    Nothing -> fail "DISCORD_TOKEN not found in environment"

  lutStore <- openLocalStateFrom lutStorePath emptyStore
  applyStore <- openLocalStateFrom applyStorePath emptyStore

  -- Run the Discord bot
  err <-
    runDiscord $
      def
        { discordToken = tok,
          discordOnStart = liftIO $ putStrLn "Started!",
          discordOnEnd = liftIO $ threadDelay (round (0.4 :: Double) * (10 ^ (6 :: Int))) >> putStrLn "\nDone!",
          discordOnEvent = eventHandler lutStore applyStore,
          discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn T.empty,
          discordGatewayIntent = def {gatewayIntentMessageContent = True}
        }
  TIO.putStrLn err

-- Event handler
eventHandler :: AcidState KeyValueStore -> AcidState KeyValueStore -> Event -> DiscordHandler ()
eventHandler lutStore applyStore event = case event of
  MessageCreate m -> unless (fromBot m) $ handleMessage lutStore applyStore m
  _ -> return ()

-- Handle incoming messages
handleMessage :: AcidState KeyValueStore -> AcidState KeyValueStore -> Message -> DiscordHandler ()
handleMessage lutStore applyStore m
  | isHelp m = sendHelpMessage m
  | isLut m = handleLutCommand lutStore applyStore m
  | otherwise = return ()

-- Send help message
sendHelpMessage :: Message -> DiscordHandler ()
sendHelpMessage m = sendMessage m helpMessage

-- Handle !lut commands
handleLutCommand :: AcidState KeyValueStore -> AcidState KeyValueStore -> Message -> DiscordHandler ()
handleLutCommand lutStore applyStore m = do
  _ <- restCall $ R.TriggerTypingIndicator (messageChannelId m)
  let parts = tail $ T.words $ messageContent m
  case parts of
    ["help"] -> sendMessage m lutHelpMessage
    ["add"] -> sendMessage m lutAddNoName
    "add" : nameParts -> handleLutAdd lutStore m nameParts
    ["rename"] -> sendMessage m lutRenameMissingArgs
    ["rename", _] -> sendMessage m lutRenameMissingArgs
    "rename" : code : nameParts -> handleLutRename lutStore m (T.toUpper code) nameParts
    ["delete"] -> sendMessage m lutDeleteNoCode
    ["delete", code] -> handleLutDelete lutStore m (T.toUpper code)
    ["list"] -> handleLutList lutStore m
    ["view"] -> sendMessage m lutViewNoCode
    ["view", code] -> handleLutView lutStore m (T.toUpper code)
    ["apply"] -> sendMessage m lutApplyNoCode
    ["apply", code] -> handleLutApply lutStore applyStore m (T.toUpper code)
    _ -> sendMessage m lutUnknownCommand

-- Handle !lut add command
handleLutAdd :: AcidState KeyValueStore -> Message -> [T.Text] -> DiscordHandler ()
handleLutAdd lutStore m nameParts = do
  let attachments = messageAttachments m
  case attachments of
    [a] -> do
      let name = T.unwords nameParts
      code <- generateUniqueCode lutStore
      liftIO $ update lutStore (InsertKeyValue code name)
      let url = attachmentUrl a
      let filename = lutFolder <> "/" <> T.unpack code <> ".png"
      success <- liftIO $ downloadFile (T.unpack url) filename
      if success
        then sendMessage m $ "Added LUT: *" <> name <> "* as **" <> code <> "**"
        else do
          _ <- liftIO $ update lutStore (RemoveKeyValue code)
          sendMessage m "Failed to download the file. Make sure you upload a valid image!"
    _ -> sendMessage m lutAddNoAttachment

-- Handle !lut rename command
handleLutRename :: AcidState KeyValueStore -> Message -> T.Text -> [T.Text] -> DiscordHandler ()
handleLutRename lutStore m code nameParts = do
  let name = T.unwords nameParts
  result <- liftIO $ query lutStore (LookupKeyValue code)
  case result of
    Just _ -> do
      liftIO $ update lutStore (RemoveKeyValue code)
      liftIO $ update lutStore (InsertKeyValue code name)
      sendMessage m $ "Renamed LUT: **" <> code <> "** to *" <> name <> "*"
    Nothing -> sendMessage m $ "LUT **" <> code <> "** not found."

-- Handle !lut delete command
handleLutDelete :: AcidState KeyValueStore -> Message -> T.Text -> DiscordHandler ()
handleLutDelete lutStore m code = do
  result <- liftIO $ query lutStore (LookupKeyValue code)
  case result of
    Just name -> do
      liftIO $ update lutStore (RemoveKeyValue code)
      let filename = lutFolder <> "/" <> T.unpack code <> ".png"
      _ <- liftIO $ removeFile filename
      sendMessage m $ "Deleted LUT: **" <> code <> "** (*" <> name <> "*)."
    Nothing -> sendMessage m $ "LUT **" <> code <> "** not found."

-- Handle !lut list command
handleLutList :: AcidState KeyValueStore -> Message -> DiscordHandler ()
handleLutList lutStore m = do
  result <- liftIO $ query lutStore AllKeyValues
  let kvs = result -- No need for `Maybe`, as `query` returns the actual result
  if Map.null kvs
    then sendMessage m "No LUTs added yet!"
    else do
      let luts = Map.toList kvs
      let lutList = T.unlines $ map (\(code, name) -> "- **" <> code <> "**: *" <> name <> "*") luts
      let content = "LUTs:\n" <> lutList
      sendMessage m content

-- Handle !lut view command
handleLutView :: AcidState KeyValueStore -> Message -> T.Text -> DiscordHandler ()
handleLutView lutStore m code = do
  result <- liftIO $ query lutStore (LookupKeyValue code)
  case result of
    Just name -> do
      let filename = lutFolder <> "/" <> T.unpack code <> ".png"
      let content = "LUT **" <> code <> "** (*" <> name <> "*):"
      sendMessageWithAttachments m content (T.pack filename)
    Nothing -> sendMessage m $ "LUT **" <> code <> "** not found."

-- Handle !lut apply command
handleLutApply :: AcidState KeyValueStore -> AcidState KeyValueStore -> Message -> T.Text -> DiscordHandler ()
handleLutApply lutStore applyStore m lutCode = do
  result <- liftIO $ query lutStore (LookupKeyValue lutCode)
  case result of
    Just lutName -> do
      let attachments = messageAttachments m
      case attachments of
        [a] -> do
          _ <- restCall $ R.CreateReaction (messageChannelId m, messageId m) "🫡"
          let lutFilename = lutFolder <> "/" <> T.unpack lutCode <> ".png"
          applyCode <- generateUniqueCode applyStore
          liftIO $ update applyStore (InsertKeyValue applyCode lutName)
          let url = attachmentUrl a
          let applyFilename = applyFolder <> "/" <> T.unpack applyCode <> ".png"
          success <- liftIO $ downloadFile (T.unpack url) applyFilename
          if success
            then do
              applySuccess <- liftIO $ applyLut lutFilename applyFilename applyFilename
              if applySuccess
                then do
                  let content = "Applied LUT: *" <> lutName <> "* (**" <> applyCode <> "**)"
                  sendMessageWithAttachments m content (T.pack applyFilename)
                else do
                  sendMessage m "Failed to apply the LUT. :skull:"
              liftIO $ removeFile applyFilename
            else do
              sendMessage m "Failed to download the file. Make sure you upload a valid image!"
          _ <- liftIO $ update applyStore (RemoveKeyValue applyCode)
          void $ restCall $ R.DeleteOwnReaction (messageChannelId m, messageId m) "🫡"
        _ -> sendMessage m lutApplyNoAttachments
    Nothing -> sendMessage m $ "LUT **" <> lutCode <> "** not found."

-- Apply the LUT
applyLut :: FilePath -> FilePath -> FilePath -> IO Bool
applyLut lutFilename filename newApplyFilename = do
  lutImageDyn <- readImage lutFilename
  case lutImageDyn of
    Left _ -> return False
    Right lut -> do
      let lutImage = convertRGBA8 lut
      inputImageDyn <- readImage filename
      case inputImageDyn of
        Left _ -> return False
        Right input -> do
          let inputImage = convertRGBA8 input
          let (width, height) = (imageWidth inputImage, imageHeight inputImage)
          liftIO $ putStrLn $ "\nCAPABILITIES: " <> show numCapabilities
          liftIO $ putStrLn $ "Starting " <> show (width, height)
          let lutPixels = [pixelAt lutImage x y | x <- [0 .. imageWidth lutImage - 1], y <- [0 .. imageHeight lutImage - 1]]
          liftIO $ putStrLn $ "LUT size: " <> show (length lutPixels)
          let lutPixelsDeduped = parallelDedup lutPixels
          liftIO $ putStrLn $ "LUT size: " <> show (length lutPixelsDeduped)
          let f x y = applyLutPixel (pixelAt inputImage x y)
          let outputImage = generateImageParallel f lutPixels width height
          liftIO $ putStrLn $ "Saving " <> newApplyFilename
          savePngImage newApplyFilename (ImageRGBA8 outputImage)
          liftIO $ putStrLn "Done!"
          return True


-- Deduplicate a list in parallel (chunk in CPU cores - 1 chunks, nub each chunk, concat the results, nub the final result)
parallelDedup :: (Ord a, NFData a) => [a] -> [a]
parallelDedup xs = 
  let
    cores = max (numCapabilities - 1) 1
    chunks = chunksOf (length xs `div` cores) xs
    dedupedChunks = parMap rdeepseq nubOrd chunks
    finalResult = concat dedupedChunks
  in
    nubOrd finalResult

-- Apply the LUT to an image in parallel
-- generateImageParallel :: (Int -> Int -> [PixelRGBA8] -> PixelRGBA8) -> [PixelRGBA8] -> Int -> Int -> Image PixelRGBA8
-- generateImageParallel f lutPixels width height =
--   let
--     rows = [[f x y lutPixels | x <- [0 .. width - 1]] | y <- [0 .. height - 1]]
--     rowsEval = withStrategy (parList (parList rdeepseq)) rows
--   in
--     generateImage (\x y -> rowsEval !! y !! x) width height

-- generateImageParallel :: (Int -> Int -> [PixelRGBA8] -> PixelRGBA8) -> [PixelRGBA8] -> Int -> Int -> Image PixelRGBA8
-- generateImageParallel f lutPixels width height =
--   let
--     rows = [[f x y lutPixels | x <- [0 .. width - 1]] | y <- [0 .. height - 1]]
--     rowsEval = withStrategy (parList (parList rdeepseq)) rows
--   in
--     -- Force full evaluation to avoid laziness after parallel execution
--     rowsEval `deepseq` generateImage (\x y -> rowsEval !! y !! x) width height

generateImageParallel :: (Int -> Int -> [PixelRGBA8] -> PixelRGBA8) -> [PixelRGBA8] -> Int -> Int -> Image PixelRGBA8
generateImageParallel f lutPixels width height =
  let
    -- rows = [[f x y lutPixels | x <- [0 .. width - 1]] | y <- [0 .. height - 1]]
    -- rowsEval = withStrategy (parList (parList rdeepseq)) rows
    pixels = [(x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]
    applied = parMap rdeepseq (\(x, y) -> f x y lutPixels) pixels
    rows = chunksOf width applied
    getPixel x y = rows !! y !! x
  in
    -- rowsEval `deepseq` generateImage (\x y -> rowsEval !! y !! x) width height
    generateImage getPixel width height

-- Apply the LUT to a single pixel. This means choosing the cloest pixel value in the LUT for each pixel in the image.
applyLutPixel :: PixelRGBA8 -> [PixelRGBA8] -> PixelRGBA8
applyLutPixel pixel = minimumBy (comparing (pixelDistance pixel))

pixelDistance :: PixelRGBA8 -> PixelRGBA8 -> Int
pixelDistance (PixelRGBA8 r1 g1 b1 _) (PixelRGBA8 r2 g2 b2 _) =
  let
    dr = fromIntegral r1 - fromIntegral r2
    dg = fromIntegral g1 - fromIntegral g2
    db = fromIntegral b1 - fromIntegral b2
  in
    dr * dr + dg * dg + db * db

-- Helper function to send a message with a reference to the original message
sendMessage :: Message -> T.Text -> DiscordHandler ()
sendMessage m content = do
  let opts =
        def
          { R.messageDetailedContent = content,
            R.messageDetailedReference = Just $ def {referenceMessageId = Just $ messageId m}
          }
  void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts)

sendMessageWithAttachments :: Message -> T.Text -> T.Text -> DiscordHandler ()
sendMessageWithAttachments m content filename = do
  bl <- liftIO $ BL.readFile (T.unpack filename)
  let bs = BL.toStrict bl
  let opts =
        def
          { R.messageDetailedContent = content,
            R.messageDetailedReference = Just $ def {referenceMessageId = Just $ messageId m},
            R.messageDetailedFile = Just (filename, bs)
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
  chars <- replicateM 3 generateChar
  return $ T.pack chars

generateChar :: IO Char
generateChar = randomRIO ('0', 'Z') >>= \c -> if c `elem` (['0' .. '9'] ++ ['A' .. 'Z']) then return c else generateChar

-- Check of a code is already used
isCodeUsed :: AcidState KeyValueStore -> T.Text -> DiscordHandler Bool
isCodeUsed lutStore code = do
  result <- liftIO $ query lutStore (LookupKeyValue code) -- Lift IO to DiscordHandler
  return $ isJust result

-- Generate a unique code
generateUniqueCode :: AcidState KeyValueStore -> DiscordHandler T.Text
generateUniqueCode lutStore = do
  code <- liftIO generateCode -- Lift IO to DiscordHandler
  used <- isCodeUsed lutStore code
  if used
    then generateUniqueCode lutStore -- Retry if the code is already used
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