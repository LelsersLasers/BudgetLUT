{-# LANGUAGE FlexibleContexts #-}
-- Allows "strings" to be Data.Text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Codec.Picture (DynamicImage (ImageRGBA8), Image (imageWidth), Pixel (pixelAt), PixelRGBA8 (..), convertRGBA8, decodeImage, encodePng, generateImage, imageHeight, pixelAt, readImage, savePngImage)
import qualified Configuration.Dotenv as Dotenv
import Control.DeepSeq (NFData (..), force)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, replicateM, unless, void, when)
import Control.Parallel.Strategies
import Data.Acid
import qualified Data.ByteString.Lazy as BL
import Data.List.Extra (nubOrd)
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Trees.KdTree
import qualified Data.Vector as V
import Discord
import qualified Discord.Requests as R
import Discord.Types
import GHC.Conc (numCapabilities)
import KeyValueStore
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, removeDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.Random
import UnliftIO (liftIO)
import UnliftIO.Concurrent

instance NFData PixelRGBA8 where
  rnf (PixelRGBA8 r g b a) = r `seq` g `seq` b `seq` a `seq` ()

instance Point PixelRGBA8 where
  dimension _ = 3 -- R, G, B as dimensions

  coord 0 (PixelRGBA8 r _ _ _) = fromIntegral r
  coord 1 (PixelRGBA8 _ g _ _) = fromIntegral g
  coord 2 (PixelRGBA8 _ _ b _) = fromIntegral b
  coord _ _ = error "Invalid coordinate"

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
lutHelpMessage :: T.Text
lutHelpMessage =
  T.pack $
    unlines
      [ "All commands:",
        "```",
        "!help",
        "!lut help",
        "!lut add <name> [with image]",
        "!lut rename <code> <new name>",
        "!lut delete <code>",
        "!lut view <code>",
        "!lut list",
        "!lut apply <code> [with image(s)]",
        "```",
        "Note: names can be multi-worded!"
      ]

-- Main function
main :: IO ()
main = do
  -- Use all cores but 1
  let cores = max (numCapabilities - 1) 1
  putStrLn $ "Using " <> show cores <> " cores."
  setNumCapabilities cores

  createDirectoryIfMissing True lutFolder
  createDirectoryIfMissing True applyFolder

  -- Clean the apply folder
  cleanApplyFolder

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

-- Clean apply folder
cleanApplyFolder :: IO ()
cleanApplyFolder = do
  subdirs <- listDirectory applyFolder
  let targetDirs = [applyFolder </> d | d <- subdirs, length d == 3]

  forM_ targetDirs $ \dir -> do
    isDir <- doesDirectoryExist dir
    when isDir $ do
      contents <- listDirectory dir
      forM_ contents $ \file -> removeFile (dir </> file)
      removeDirectory dir

  putStrLn "Cleaned apply folder."

-- Event handler
eventHandler :: AcidState KeyValueStore -> AcidState KeyValueStore -> Event -> DiscordHandler ()
eventHandler lutStore applyStore event = case event of
  MessageCreate m -> unless (fromBot m) $ handleMessage lutStore applyStore m
  _ -> return ()

-- Handle incoming messages
handleMessage :: AcidState KeyValueStore -> AcidState KeyValueStore -> Message -> DiscordHandler ()
handleMessage lutStore applyStore m
  | isMessagePrefix "!help" m = sendMessage m "Use `!lut help` to see all the `!lut` commands."
  | isMessagePrefix "!lut" m = handleLutCommand lutStore applyStore m
  | otherwise = return ()

-- Handle !lut commands
handleLutCommand :: AcidState KeyValueStore -> AcidState KeyValueStore -> Message -> DiscordHandler ()
handleLutCommand lutStore applyStore m = do
  void $ restCall $ R.TriggerTypingIndicator (messageChannelId m)
  let parts = tail $ T.words $ messageContent m
  case parts of
    ["help"] -> sendMessage m lutHelpMessage
    ["add"] -> sendMessage m "You need to provide a name for the lut. Use `!lut add <name>`."
    "add" : nameParts -> handleLutAdd lutStore m nameParts
    ["rename"] -> sendMessage m "You need to provide the code and the new name for the lut. Use `!lut rename <code> <new name>`."
    ["rename", _] -> sendMessage m "You need to provide the code and the new name for the lut. Use `!lut rename <code> <new name>`."
    "rename" : code : nameParts -> handleLutRename lutStore m (T.toUpper code) nameParts
    ["delete"] -> sendMessage m "You need to provide the code of the lut you want to delete. Use `!lut delete <code>`."
    ["delete", code] -> handleLutDelete lutStore m (T.toUpper code)
    ["list"] -> handleLutList lutStore m
    ["view"] -> sendMessage m "You need to provide the code of the lut you want to view. Use `!lut view <code>`."
    ["view", code] -> handleLutView lutStore m (T.toUpper code)
    ["apply"] -> sendMessage m "You need to provide the code of the lut you want to apply. Use `!lut apply <code>`."
    ["apply", code] -> handleLutApply lutStore applyStore m (T.toUpper code)
    _ -> sendMessage m "Unknown command. Use `!lut help` for all the available commands."

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
        then sendMessage m $ "Added LUT: *" <> name <> "* as **" <> code <> "**."
        else do
          liftIO $ update lutStore (RemoveKeyValue code)
          sendMessage m "Failed to download and process the file. Make sure you upload a valid image."
    _ -> sendMessage m "You need to provide exactly one image attachment for the lut."

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
      liftIO $ removeFile filename
      sendMessage m $ "Deleted LUT: **" <> code <> "** (*" <> name <> "*)."
    Nothing -> sendMessage m $ "LUT **" <> code <> "** not found."

-- Handle !lut list command
handleLutList :: AcidState KeyValueStore -> Message -> DiscordHandler ()
handleLutList lutStore m = do
  result <- liftIO $ query lutStore AllKeyValues
  let kvs = result
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
        [] -> sendMessage m "You need to provide at least one image attachment to apply the LUT to."
        as -> do
          void $ restCall $ R.CreateReaction (messageChannelId m, messageId m) "🫡"

          let lutFilename = lutFolder <> "/" <> T.unpack lutCode <> ".png"
          lutTree <- readLutImage lutFilename
          case lutTree of
            Nothing -> sendMessage m "Failed to read the LUT image. Try deleting and re-adding it." -- Shouldn't happen
            Just lt -> do
              applyCode <- generateUniqueCode applyStore
              liftIO $ update applyStore (InsertKeyValue applyCode "")
              let folder = applyFolder <> "/" <> T.unpack applyCode
              multiapply folder as lt lutCode lutName m
              liftIO $ update applyStore (RemoveKeyValue applyCode)
              liftIO $ removeDirectory folder

          void $ restCall $ R.DeleteOwnReaction (messageChannelId m, messageId m) "🫡"
    Nothing -> sendMessage m $ "LUT **" <> lutCode <> "** not found."

-- Multiapply function
multiapply :: String -> [Attachment] -> KdTree PixelRGBA8 -> T.Text -> T.Text -> Message -> DiscordHandler ()
multiapply folder attachments lutTree lutCode lutName m = do
  case attachments of
    [] -> return ()
    attachment : rest -> do
      let url = attachmentUrl attachment
      let applyFilename = folder <> "/" <> T.unpack (attachmentFilename attachment)
      success <- liftIO $ downloadFile (T.unpack url) applyFilename

      if success
        then do
          applySuccess <- liftIO $ applyLut lutTree applyFilename
          if applySuccess
            then do
              let content = "Applied LUT *" <> lutName <> "* (**" <> lutCode <> "**) to `" <> attachmentFilename attachment <> "`:"
              sendMessageWithAttachments m content (T.pack applyFilename)
            else do
              sendMessage m "Failed to read the input. Try again." -- Shouldn't happen
          liftIO $ removeFile applyFilename
        else do
          sendMessage m $ "Failed to download and process: `" <> attachmentFilename attachment <> "`."

      multiapply folder rest lutTree lutCode lutName m

-- Read lut image
readLutImage :: FilePath -> DiscordHandler (Maybe (KdTree PixelRGBA8))
readLutImage filename = do
  lutImageDyn <- liftIO $ readImage filename
  case lutImageDyn of
    Left _ -> return Nothing
    Right lut -> do
      let lutImage = convertRGBA8 lut
      let width = imageWidth lutImage
      let height = imageHeight lutImage
      let lutPixels = [pixelAt lutImage x y | x <- [0 .. width - 1], y <- [0 .. height - 1]]
      let lutPixelsDeduped = parallelDedup lutPixels
      return $ Just $ fromList lutPixelsDeduped

-- Apply the LUT to an image
applyLut :: KdTree PixelRGBA8 -> FilePath -> IO Bool
applyLut lutTree filename = do
  inputImageDyn <- readImage filename
  case inputImageDyn of
    Left _ -> return False
    Right input -> do
      let inputImage = convertRGBA8 input
      let (width, height) = (imageWidth inputImage, imageHeight inputImage)
      let f x y = applyLutPixel (pixelAt inputImage x y)
      let outputImage = generateImageParallel f lutTree width height
      savePngImage filename (ImageRGBA8 outputImage)
      return True

-- Deduplicate a list in parallel (chunk in CPU cores - 1 chunks, nub each chunk, concat the results, nub the final result)
parallelDedup :: (Ord a, NFData a) => [a] -> [a]
parallelDedup xs =
  let cores = max (numCapabilities - 1) 1
      chunks = chunksOf (length xs `div` cores) xs
      dedupedChunks = parMap rdeepseq nubOrd chunks
      finalResult = concat dedupedChunks
   in nubOrd finalResult

generateImageParallel :: (Int -> Int -> KdTree PixelRGBA8 -> PixelRGBA8) -> KdTree PixelRGBA8 -> Int -> Int -> Image PixelRGBA8
generateImageParallel f lutPixels width height =
  let pixels = [f x y lutPixels | y <- [0 .. height - 1], x <- [0 .. width - 1]]
      applied = force $ using pixels $ parListChunk 1000 rdeepseq
      appliedVec = V.fromList applied
   in generateImage (\x y -> appliedVec V.! (y * width + x)) width height

-- Apply the LUT to a single pixel. This means choosing the cloest pixel value in the LUT for each pixel in the image.
applyLutPixel :: PixelRGBA8 -> KdTree PixelRGBA8 -> PixelRGBA8
applyLutPixel pixel lutTree =
  let (PixelRGBA8 r g b _) = pixel
      bestPixel = Data.Maybe.fromMaybe pixel (nearestNeighbor lutTree (PixelRGBA8 r g b 0))
   in bestPixel

pixelDistance :: PixelRGBA8 -> PixelRGBA8 -> Int
pixelDistance (PixelRGBA8 r1 g1 b1 _) (PixelRGBA8 r2 g2 b2 _) =
  let dr = fromIntegral r1 - fromIntegral r2
      dg = fromIntegral g1 - fromIntegral g2
      db = fromIntegral b1 - fromIntegral b2
   in dr * dr + dg * dg + db * db

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
    let content = getResponseBody response
    case decodeImage content of
      Left _ -> return False
      Right dynamicImage -> do
        let image = convertRGBA8 dynamicImage
        BL.writeFile filename (encodePng image)
        return True
  case result of
    Left (_ :: SomeException) -> return False
    Right success -> return success

-- Generate a 3 long code that contains numbers or capital letters
generateCode :: IO T.Text
generateCode = do
  chars <- replicateM 3 generateChar
  return $ T.pack chars

generateChar :: IO Char
generateChar = do
  c <- randomRIO ('0', 'Z')
  if c `elem` (['0' .. '9'] ++ ['A' .. 'Z'])
    then return c
    else generateChar

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

-- Check if a message starts with a certain prefix
isMessagePrefix :: T.Text -> Message -> Bool
isMessagePrefix prefix = T.isPrefixOf prefix . T.toLower . messageContent