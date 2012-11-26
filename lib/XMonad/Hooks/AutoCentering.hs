--{-# LANGUAGE MultiParamTypeClasses, Rank2Types #-}
module XMonad.Hooks.AutoCentering where
import Control.Monad.Reader
import Data.Bits
import Data.Monoid
import XMonad
import XMonad.StackSet (floating, member)
import XMonad.Util.WindowProperties

import qualified Data.Map             as M

autoCenteringEventHook :: Property -> Event -> X All
autoCenteringEventHook prop e@(ConfigureRequestEvent {ev_window = w}) = withDisplay $ \dpy -> do
    isProp <- hasProperty prop w
    ws <- gets windowset
    wa <- io $ getWindowAttributes dpy w
    bw <- asks (borderWidth . config)
    
    let screenWidth  = displayWidth  dpy (defaultScreen dpy)
        screenHeight = displayHeight dpy (defaultScreen dpy)
        mask = ev_value_mask e .|. 3

    if (isProp && (M.member w (floating ws) || not (member w ws)))
        then
          do io $ configureWindow dpy w mask $ WindowChanges
                      { wc_x            = div (screenWidth - (ev_width e)) 2
                      , wc_y            = div (screenHeight -  (ev_height e)) 2
                      , wc_width        = ev_width e
                      , wc_height       = ev_height e
                      , wc_border_width = fromIntegral bw
                      , wc_sibling      = ev_above e
                      , wc_stack_mode   = ev_detail e }
             when (member w ws) (float w)
             return $ All False
        else return $ All True

autoCenteringEventHook _ _ = return (All True)

