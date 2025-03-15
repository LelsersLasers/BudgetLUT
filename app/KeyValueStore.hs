{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module KeyValueStore where

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Acid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.SafeCopy
import Data.Text (Text)
import Data.Typeable

newtype KeyValueStore
  = KeyValueStore (Map Text Text) -- Code -> name
  deriving (Show, Typeable)

-- Initialize an empty store
emptyStore :: KeyValueStore
emptyStore = KeyValueStore Map.empty

-- Define operations
insertKeyValue :: Text -> Text -> Update KeyValueStore ()
insertKeyValue key value = do
  KeyValueStore kvs <- get
  put $ KeyValueStore $ Map.insert key value kvs

removeKeyValue :: Text -> Update KeyValueStore ()
removeKeyValue key = do
  KeyValueStore kvs <- get
  put $ KeyValueStore $ Map.delete key kvs

lookupKeyValue :: Text -> Query KeyValueStore (Maybe Text)
lookupKeyValue key = do
  KeyValueStore kvs <- ask
  return $ Map.lookup key kvs

-- Make the state serializable
$(deriveSafeCopy 0 'base ''KeyValueStore)

-- Create AcidState instance
$(makeAcidic ''KeyValueStore ['insertKeyValue, 'removeKeyValue, 'lookupKeyValue])