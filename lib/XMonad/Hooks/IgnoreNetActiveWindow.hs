module XMonad.Hooks.IgnoreNetActiveWindow where

import Data.Monoid
import XMonad

-- | Ignore window activation requests from some windows, e.g. a browser
-- stealing focus whenever a link is opened from another app.
--
-- Usage:
--
-- > main = xmonad $ ignoreNetActiveWindow q $ ewmh def
-- >   where
-- >     q = className =? "google-chrome"
ignoreNetActiveWindow :: Query Bool -> XConfig a -> XConfig a
ignoreNetActiveWindow q c =
  c { handleEventHook = ignoreNetActiveWindowEventHook q (handleEventHook c) }

ignoreNetActiveWindowEventHook :: Query Bool -> (Event -> X All) -> Event -> X All
ignoreNetActiveWindowEventHook q hook e@ClientMessageEvent{ ev_window = w, ev_message_type = mt } = do
  a_aw <- getAtom "_NET_ACTIVE_WINDOW"
  if mt == a_aw
    then do ignore <- runQuery q w
            if ignore
              then return (All True)
              else hook e
    else hook e
ignoreNetActiveWindowEventHook _ hook e = hook e
