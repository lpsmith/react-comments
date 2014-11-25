{-# LANGUAGE LambdaCase, DoAndIfThenElse, OverloadedStrings #-}

module Reload where

import qualified System.Linux.Inotify as IN
import           Control.Concurrent.STM
import qualified Control.Concurrent.STM.TChan.Split as C
import qualified Data.ByteString.Char8 as B8
import           Control.Exception(bracket)
import           Control.Concurrent(forkIO)
import           Control.Monad(forever)
import           Control.Applicative
import           Data.Monoid(mconcat)
import           Data.Function(fix)

-- type FileReload = (MVar (HashMap IN.Watch RawFileName), TChan RawFileName)

type FileReload = C.SendPort ()

events :: IN.Mask a
events = mconcat [
    IN.in_DELETE
  , IN.in_MODIFY
  , IN.in_MOVE
  , IN.in_CREATE
  ]


initFileReload :: String -> IO FileReload
initFileReload dir = do
  c <- atomically $ C.newSendPort
  forkIO $ bracket IN.init IN.close $ \ind -> do
    IN.addWatch ind dir events
    forever $ do
      e <- IN.getEvent ind
      print e
      let name = IN.name e
      if B8.null name
         || (B8.head name == '#' && B8.last name == '#')
         || B8.last name == '~'
         || (B8.isPrefixOf ".#" name)
      then return ()
      else do
        fix $ \loop -> do
           IN.getEventFromBuffer ind >>= \case
             Nothing -> return ()
             Just e  -> print e >> loop
        atomically $ C.send c ()
  return c

listenForReload :: FileReload -> IO (STM ())
listenForReload sp = do
  rp <- atomically $ C.listen sp
  return $ C.receive rp
