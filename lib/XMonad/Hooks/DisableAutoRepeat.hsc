{-# LANGUAGE ForeignFunctionInterface #-}

-- Warning: requires patching xmonad core

module XMonad.Hooks.DisableAutoRepeat (disableAutoRepeat) where

import Foreign
import Foreign.C.Types

import XMonad

#include <X11/XKBlib.h>

foreign import ccall unsafe "X11/XKBlib.h XkbSetDetectableAutoRepeat"
	xkbSetDetectableAutoRepeat :: Display -> CInt -> Ptr CInt -> IO CInt

disableAutoRepeat :: X ()
disableAutoRepeat = withDisplay $ \dpy -> do
	io (xkbSetDetectableAutoRepeat dpy 1 nullPtr) >> return ()
