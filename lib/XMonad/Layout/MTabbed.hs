{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  MTabbed
-- Copyright   :  (c) 2007 David Roundy, Andrea Rossato
-- Copyright   :  (c) 2011 Evgeny I. E. Omelchenko
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  elemir90 at gmail dot com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A multiply mtabbed layout for the Xmonad Window Manager
--
-----------------------------------------------------------------------------

module XMonad.Layout.MTabbed
    ( simpleMTabbed, mtabbed, addMTabs
    , Theme (..)
    , defaultTheme
    , MTabbedDecoration (..)
    , shrinkText, CustomShrink(CustomShrink)
    , Shrinker(..)
    , nameDeco, CustomNamer(CustomNamer)
    , Namer(..)
    ) where

import Data.IORef
import Data.List
import Data.Int (Int32)

import System.IO.Unsafe

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Layout.DecorationNG

import XMonad.Layout.Simplest ( Simplest(Simplest) )

-- Layouts

-- | A mtabbed layout with the default xmonad Theme.
simpleMTabbed :: Int32 -> ModifiedLayout (Decoration MTabbedDecoration DefaultNamer DefaultShrinker) Simplest Window
simpleMTabbed count = mtabbed count nameDeco shrinkText defaultTheme

-- | A layout decorated with tabs and the possibility to set a custom
-- shrinker and theme.
mtabbed :: (Eq a, Namer n, Shrinker s) => Int32 -> n -> s -> Theme
        -> ModifiedLayout (Decoration MTabbedDecoration n s) Simplest a
mtabbed count n s c = addMTabs count n s c Simplest

-- Layout Modifiers

-- | A layout modifier that uses the provided shrinker and theme to add tabs to any layout.
addMTabs :: (Eq a, LayoutClass l a, Namer n, Shrinker s) => Int32 -> n -> s -> Theme -> l a
         -> ModifiedLayout (Decoration MTabbedDecoration n s) l a
addMTabs count = createMTabs count

-- Tab creation abstractions.  Internal use only.

-- Create tabbar when required at the given location with the given
-- shrinker and theme to the supplied layout.
createMTabs :: (Eq a, LayoutClass l a, Namer n, Shrinker s) => Int32 -> n -> s
            -> Theme -> l a -> ModifiedLayout (Decoration MTabbedDecoration n s) l a
createMTabs count n tx th l = decoration n tx th (MTabbed count) l

data MTabbedDecoration a = MTabbed Int32 deriving (Show, Read)

instance Eq a => DecorationStyle MTabbedDecoration a where
    describeDeco (MTabbed _) = "MTabbed"
    decorationEventHook _ ds ButtonEvent { ev_window     = ew
                                         , ev_event_type = et
                                         , ev_button     = eb }
        | et == buttonPress
        , Just ((w,_),_) <- findWindowByDecoration ew ds =
           if eb == button2
               then killWindow w
               else focus w
    decorationEventHook _ _ _ = return ()

    pureDecoration ds _ ht _ s wrs (w,r@(Rectangle x y wh hh))
       = let 
           MTabbed count = ds
           ws = filter (`elem` map fst (filter ((==r) . snd) wrs)) (S.integrate s)
           numWindows = fi $ length ws
           rr = 1 + ((numWindows - 1) `div` count)
           wno = fi $ maybe 0 id $ w `elemIndex` ws
           row = wno `div` count
           nx = x + (fi nw) * (wno `mod` fi count)
           ny = y + (fi ht) * row
           nw = (fi wh) `div` (fi (max 1 (min count (numWindows - fi row * count))))
           dwr = Rectangle nx ny nw (fi ht)
           wr = Rectangle x (y + (fi rr) * fi ht) wh (hh - (fi rr) * fi ht)
           in
             Just (wr, dwr)

