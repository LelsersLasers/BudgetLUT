{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

module Main where

import qualified Configuration.Dotenv as Dotenv
import System.Environment (lookupEnv)

import Control.Monad (when, void, unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import UnliftIO (liftIO)
import UnliftIO.Concurrent

import Discord
import Discord.Types
import qualified Discord.Requests as R

help :: T.Text
help = "Use !lut help for more specifics about the !lut command"

lutHelp :: T.Text
lutHelp = "Use !lut <command> for more specifics about the !lut command"

lutUnknown :: T.Text
lutUnknown = "Unknown command. Use !lut help for more specifics about the !lut command"

lutAddNoName :: T.Text
lutAddNoName = "You need to provide a name for the lut. Use !lut add <name of lut>"

lutAddNoAttachment :: T.Text
lutAddNoAttachment = "You need to provide exactly one attachment for the lut."



main :: IO ()
main = do
  -- Load environment variables from .env file
  Dotenv.loadFile Dotenv.defaultConfig

  -- Get the DISCORD_TOKEN from the environment
  maybeToken <- lookupEnv "DISCORD_TOKEN"
  tok <- case maybeToken of
    Just token -> return (T.pack token)
    Nothing -> fail "DISCORD_TOKEN not found in environment"

  err <- runDiscord $ def { discordToken = tok
                        --   , discordOnStart = startHandler testserverid
                          , discordOnEnd = liftIO $ threadDelay (round (0.4 :: Double) * (10 ^ (6 :: Int))) >>  putStrLn "Ended"
                          , discordOnEvent = eventHandler
                          , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn T.empty
                          , discordGatewayIntent = def {
                                                        -- gatewayIntentGuilds = True
                                                      --  , gatewayIntentMembers = True
                                                       gatewayIntentMessageContent = True
                                                      --  , gatewayIntentMessageReactions = True
                                                      --  , gatewayIntentMessageChanges = True
                                                       }
                          }
  TIO.putStrLn err

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
      MessageCreate m -> unless (fromBot m) $ do
        when (isHelp m) $ do
          let opts :: R.MessageDetailedOpts
              opts = def { R.messageDetailedContent = help
                         , R.messageDetailedReference = Just $
                            def { referenceMessageId = Just $ messageId m }
                         }
          void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts)
        when (isLut m) $ do
          handleLut m
          -- void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
          -- threadDelay (2 * 10 ^ (6 :: Int))

          -- -- A very simple message.
          -- Right m' <- restCall (R.CreateMessage (messageChannelId m) "Pong")
          -- void $ restCall (R.EditMessage (messageChannelId m, messageId m') (def {R.messageDetailedContent=messageContent m' <> "!"}))

          -- latency <- getGatewayLatency
          -- mLatency <- measureLatency

          -- -- A more complex message. Text-to-speech, does not mention everyone nor
          -- -- the user, and uses Discord native replies.
          -- -- Use ":info" in ghci to explore the type
          -- let opts :: R.MessageDetailedOpts
          --     opts = def { R.messageDetailedContent = "Here's a more complex message, but doesn't ping @everyone!. Here's the current gateway latency: " <> (T.pack . show) ([latency, mLatency])
          --                , R.messageDetailedTTS = True
          --                , R.messageDetailedAllowedMentions = Just $
          --                   def { R.mentionEveryone = False
          --                       , R.mentionRepliedUser = False
          --                       }
          --                , R.messageDetailedReference = Just $
          --                   def { referenceMessageId = Just $ messageId m }
          --                }
          -- void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts)
      _ -> return ()

handleLut :: Message -> DiscordHandler ()
handleLut m = do
  let parts = tail $ T.words $ messageContent m
  let originalM = Just $ def { referenceMessageId = Just $ messageId m }
  case parts of
    ["help"] -> do
      let opts :: R.MessageDetailedOpts
          opts = def { R.messageDetailedContent = lutHelp
                     , R.messageDetailedReference = originalM
                     }
      void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts)
    ["add"] -> do
      let opts :: R.MessageDetailedOpts
          opts = def { R.messageDetailedContent = lutAddNoName
                     , R.messageDetailedReference = originalM
                     }
      void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts) 
    "add" : nameParts -> do
      let attachments = messageAttachments m
      case attachments of 
        [a] -> do
          let name = T.unwords nameParts
          let opts :: R.MessageDetailedOpts
              opts = def { R.messageDetailedContent = "Adding " <> attachmentUrl a <> " as " <> name
                        , R.messageDetailedReference = originalM
                        }
          void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts) 
        _  -> do
          let opts :: R.MessageDetailedOpts
              opts = def { R.messageDetailedContent = lutAddNoAttachment
                        , R.messageDetailedReference = originalM
                        }
          void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts)
    _  -> do
      let opts :: R.MessageDetailedOpts
          opts = def { R.messageDetailedContent = lutUnknown
                     , R.messageDetailedReference = originalM
                     }
      void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts)

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isLut :: Message -> Bool
isLut = ("!lut" `T.isPrefixOf`) . T.toLower . messageContent

isHelp :: Message -> Bool
isHelp = ("!help" `T.isPrefixOf`) . T.toLower . messageContent