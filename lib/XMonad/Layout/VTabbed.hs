{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  VTabbed
-- Copyright   :  (c) 2007 David Roundy, Andrea Rossato
-- Copyright   :  (c) 2011 Evgeny I. E. Omelchenko
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  elemir90 at gmail dot com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A multiply vtabbed layout for the Xmonad Window Manager
--
-----------------------------------------------------------------------------

module XMonad.Layout.VTabbed
    ( simpleVTabbed, vtabbed, addVTabs
    , Theme (..)
    , defaultTheme
    , VTabbedDecoration (..)
    , shrinkText, CustomShrink(CustomShrink)
    , Shrinker(..)
    , nameDeco, CustomNamer(CustomNamer)
    , Namer(..)
    ) where

import Data.Maybe
import Data.List
import Data.Int (Int32)

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Layout.DecorationNG
import XMonad.Layout.Simplest ( Simplest(Simplest) )

-- Layouts

-- | A vtabbed layout with the default xmonad Theme.
simpleVTabbed :: Int32 -> ModifiedLayout (Decoration VTabbedDecoration DefaultNamer DefaultShrinker) Simplest Window
simpleVTabbed size = vtabbed size nameDeco shrinkText defaultTheme

-- | A layout decorated with tabs and the possibility to set a custom
-- shrinker and theme.
vtabbed :: (Eq a, Namer n, Shrinker s) => Int32 -> n -> s -> Theme
        -> ModifiedLayout (Decoration VTabbedDecoration n s) Simplest a
vtabbed size n s c = addVTabs size n s c Simplest

-- Layout Modifiers

-- | A layout modifier that uses the provided shrinker and theme to add tabs to any layout.
addVTabs :: (Eq a, LayoutClass l a, Namer n, Shrinker s) => Int32 -> n -> s -> Theme -> l a
         -> ModifiedLayout (Decoration VTabbedDecoration n s) l a
addVTabs size = createVTabs size

-- Tab creation abstractions.  Internal use only.

-- Create tabbar when required at the given locationith the given
-- shrinker and theme to the supplied layout.
createVTabs :: (Eq a, LayoutClass l a, Namer n, Shrinker s) => Int32 -> n -> s
            -> Theme -> l a -> ModifiedLayout (Decoration VTabbedDecoration n s) l a
createVTabs size n tx th l = decoration n tx th (VTabbed size) l

data VTabbedDecoration a = VTabbed Int32 deriving (Read, Show)

instance Eq a => DecorationStyle VTabbedDecoration a where
    describeDeco (VTabbed _) = "VTabbed"

    decorationEventHook _ ds ButtonEvent { ev_window     = ew
                                             , ev_event_type = et
                                             , ev_button     = eb }
        | et == buttonPress
        , Just ((w,_),_) <-findWindowByDecoration ew ds =
           if eb == button2
               then killWindow w
               else focus w
    decorationEventHook _ _ _ = return ()

    pureDecoration (VTabbed size) _ ht _ s wrs (w,r@(Rectangle x y wh hh))
        = Just $ (Rectangle (x + fi size) y (wh - fi size) hh, Rectangle 0 (fi ny) (fi size) ht)
        where ws = filter (`elem` map fst (filter ((==r) . snd) wrs)) (S.integrate s)
              loc i = y + fi (ht * (fi i))
              ny  = maybe y loc $ w `elemIndex` ws
              numWindows = length ws

