{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Comments
import Reload
import Snap.Snaplet
-- import Snap.Snaplet.Heist
-- import Snap.Snaplet.Auth
-- import Snap.Snaplet.Session

------------------------------------------------------------------------------
data App = App
    { 
{--
      _heist :: Snaplet (Heist App),
      _sess :: Snaplet SessionManager,
      _auth :: Snaplet (AuthManager App),
--}
      _comments :: Comments,
      _reload   :: FileReload
    }

makeLenses ''App

{--
instance HasHeist App where
    heistLens = subSnaplet heist
--}

------------------------------------------------------------------------------
type AppHandler = Handler App App
