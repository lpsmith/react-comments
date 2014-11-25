{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Comments where

import qualified Data.Text as T
import qualified Data.IntMap.Strict as IM
import           Control.Concurrent.STM
import           Data.Json.Builder
import           Data.Monoid


data Comment = Comment {
    author :: !T.Text,
    text   :: !T.Text
}

type Comments = TVar (IM.IntMap Comment)

initComments :: IO Comments
initComments = newTVarIO IM.empty

getCommentGE :: Comments -> IM.Key -> STM (IM.Key, Comment)
getCommentGE csRef k = do
      cs <- readTVar csRef
      case IM.lookupGE k cs of
        Nothing -> retry
        Just c  -> return c

putComment :: Comments -> Comment -> STM ()
putComment csRef c = do
      cs <- readTVar csRef
      case IM.maxViewWithKey cs of
        Nothing -> writeTVar csRef (IM.singleton 0 c)
        Just ((k,_),_) -> writeTVar csRef (IM.insert (k + 1) c cs)

instance JsObject Comment where
    toObject c = row ("author"::T.Text) (author c)
              <> row ("text"::T.Text) (text c)

instance Value Comment where
    toJson = toJson . toObject
