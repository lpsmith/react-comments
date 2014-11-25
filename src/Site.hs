{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
-- import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Snap.Core
import           Snap.Snaplet
-- import           Snap.Snaplet.Auth
-- import           Snap.Snaplet.Auth.Backends.JsonFile
-- import           Snap.Snaplet.Heist
-- import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
-- import           Heist
-- import qualified Heist.Interpreted as I
import           Data.Monoid
import           Data.Json.Builder
import           Control.Concurrent.STM
import           Blaze.ByteString.Builder
import           Blaze.Text.Int
import           Control.Monad.State
-- import           Control.Monad.Trans (liftIO)
import qualified System.IO.Streams as Streams
import           Safe (readMay)

import           Comments
import           Reload
------------------------------------------------------------------------------
import           Application

{--
------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"
--}

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [
--         ("/login",    with auth handleLoginSubmit)
--       , ("/logout",   with auth handleLogout)
--       , ("/new_user", with auth handleNewUser)
           ("comments.json",  handleComments)
         , ("",          serveDirectory "static")
         ]

handleComments :: Handler App App ()
handleComments = method GET getComments <|> method POST postComment

getComments :: Handler App App ()
getComments = do
  cs <- gets _comments
  rl <- liftIO . listenForReload =<< gets _reload
  last_event_id <- getHeader "Last-Event-ID" <$> getRequest
  let k::Int = maybe (-1) id (readMay =<< (B8.unpack <$> last_event_id))
  setTimeout 3600
  modifyResponse ( setResponseStatus 200 "OK"
                 . setHeader "Connection" "keep-alive"
                 . setHeader "Cache-Control" "no-cache"
                 . setContentType "text/event-stream"
                 . setResponseBody (go k cs rl) )
  where
    go k cs fileModified out = do
--       Streams.write (Just $ fromByteString "retry: 1000\n\n") out
        loop (k+1)
      where
        loop k = do
          mc <- atomically $  ( Just <$> getCommentGE cs k     )
                     `orElse` ( fileModified >> return Nothing )
          case mc of
            Nothing -> do
                Streams.write (Just $ fromByteString "event: reload\ndata: \n\n") out
                return out
            Just (k',c) -> do
                Streams.write (Just $ formatComment k c) out
                loop $! (k' + 1)

    formatComment k c =
       fromByteString "id: " <> integral k <>
       fromByteString "\nevent: comment\ndata: " <> toBuilder (toJson c) <> fromByteString "\n\n" <> flush


postComment :: Handler App App ()
postComment = do
  author <- maybe "" TE.decodeUtf8 <$> getParam "author"
  text   <- maybe "" TE.decodeUtf8 <$> getParam "text"
  cs <- gets _comments
  liftIO $ atomically $ putComment cs (Comment author text)


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
{--
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    addAuthSplices h auth
--}
    addRoutes routes

    c <- liftIO $ initComments
    r <- liftIO $ initFileReload "static"

    return $ App c r
