--{{{ Imports
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, NoMonomorphismRestriction, OverloadedStrings, DeriveDataTypeable #-}

import Control.Concurrent
import Control.Concurrent.MVar
import qualified Control.Exception as C
import Control.Monad
import Data.List
import qualified Data.Map        as M
import Data.Maybe
import Data.Monoid
import Data.Ratio ((%))
import qualified Data.Text as T
import Numeric
import System.Exit
import System.IO
import System.Posix.POpen
import System.Posix.Signals
import System.Posix.Types
import System.Process
--import Debug.Trace

import XMonad hiding (trace)
import XMonad.Actions.FocusNth
import XMonad.Actions.GridSelect
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import qualified XMonad.Layout.Decoration
import qualified XMonad.Layout.DecorationMadness
import qualified XMonad.Layout.DwmStyle as DWM
import XMonad.Layout.Grid
--import XMonad.Layout.GridVariants
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.Monitor
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizeScreen
import XMonad.Layout.SimpleFloat
--import XMonad.Layout.TabBarDecoration
--import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.StackSet hiding (workspaces, focus)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.Scratchpad

-- Local libraries
import XMonad.Hooks.DisableAutoRepeat
import XMonad.Hooks.IgnoreNetActiveWindow
import XMonad.Hooks.PerWindowKbdLayout
import XMonad.Layout.MTabbed as MT
import XMonad.Layout.VTabbed as VT
--}}}
--{{{ Compton
import DBus
import DBus.Client
import Data.Word
import Graphics.X11.Xlib.Display

dpyName :: Display -> String
dpyName dpy = map replace $ displayString dpy where
  replace ':' = '_'
  replace '.' = '_'
  replace c = c

inversionStatus :: Display -> Window -> X Bool
inversionStatus dpy w =
  let mc = (methodCall "/" "com.github.chjj.compton" "win_get")
             { methodCallDestination = Just $ busName_ $ "com.github.chjj.compton." ++ dpyName dpy
             , methodCallBody = [toVariant (fromIntegral w :: Word32)
                                , toVariant ("invert_color_force" :: String)
                                ]
             }
  in io $ do client <- connectSession
             status <- call_ client mc
             disconnect client
             return $ (/= 0) $ fromJust $ (fromVariant :: Variant -> Maybe Word16) $ head $ methodReturnBody status

invert :: Display -> Window -> Bool -> X ()
invert dpy w status =
  let mc = (methodCall "/" "com.github.chjj.compton" "win_set")
             { methodCallDestination = Just $ busName_ $ "com.github.chjj.compton." ++ dpyName dpy
             , methodCallBody = [toVariant (fromIntegral w :: Word32)
                                , toVariant ("invert_color_force" :: String)
                                , toVariant ((if status then 1 else 0) :: Word16)
                                ]
             }
  in io $ do client <- connectSession
             callNoReply client mc
             disconnect client
--}}}
--{{{ A CustomNamer to include the window numbers

instance Namer CustomNamer where
    nameIt _ w = do
                    ws <- gets windowset
                    nw <- getName w
                    let num = maybe "" (\x -> (show x) ++ ":") $ elemIndex w (W.integrate' $ W.stack
                                                                          $ W.workspace $ W.current ws)
                    return $ num ++ (show nw)

myName = CustomNamer
--}}}
--{{{ Theme
myTheme = defaultTheme {
	fontName = "xft:Terminus:size=10",
	--fontName = "-*-terminus-medium-*-*-*-14-*-*-*-*-*-iso10646-*",
	activeColor = "#000000",
	inactiveColor = "#1A1A1A",
	urgentColor = "#330000",
	activeTextColor = "#FFFF00",
	inactiveTextColor = "#BBBBBB",
	urgentTextColor = "#FF0000",
	activeBorderColor = "#00FF00",
	inactiveBorderColor = "#555555",
	urgentBorderColor = "#FF0000",
	decoWidth = 1600,
	decoHeight = 18
}
myXPConfig = defaultXPConfig {
	font = fontName myTheme,
	bgColor = activeColor myTheme,
	fgColor = activeTextColor myTheme,
	borderColor = activeBorderColor myTheme
}
--}}}
--{{{ Scratchpads
scratchpads = [
	NS "vimscratch" "urxvt -name vimscratch -e vim ~/vdoc/scratchpad" (resource =? "vimscratch") nonFloating]
--}}}
--{{{ Key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
	[ ((modm .|. shiftMask,		xK_c),		kill)
	, ((modm,			xK_space),	sendMessage NextLayout)
	, ((modm .|. shiftMask,		xK_space),	setLayout $ XMonad.layoutHook conf)
	, ((modm,			xK_n),		refresh)
	, ((modm,			xK_m),		windows focusMaster)
	, ((modm,			xK_Return),	windows swapMaster)
	, ((modm .|. controlMask,	xK_Return),	mkTerm)
	, ((modm .|. controlMask,	xK_h),		sendMessage Shrink)
	, ((modm .|. controlMask,	xK_l),		sendMessage Expand)
	, ((modm,			xK_t),		withFocused $ windows . sink)
	, ((modm,			xK_comma),	sendMessage (IncMasterN 1))
	, ((modm,			xK_period),	sendMessage (IncMasterN (-1)))
	, ((modm .|. shiftMask,		xK_q),		io (exitWith ExitSuccess))
	, ((modm,			xK_u),		focusUrgent)
	, ((modm,			xK_i     ),	withDisplay $ \dpy -> withFocused $ \w -> inversionStatus dpy w >>= \status -> invert dpy w $ not status)
	, ((modm,			xK_g),		windowPromptGoto myXPConfig
								{ searchPredicate = fuzzyMatch
								, sorter = fuzzySort
								})
	-- Leave the vanilla focusing bindings to work with Full
	, ((modm,			xK_Down),	windows focusDown)
	, ((modm,			xK_Up),		windows focusUp)
	, ((modm .|. shiftMask,		xK_Down),	windows swapDown)
	, ((modm .|. shiftMask,		xK_Up),		windows swapUp)
	, ((modm,			xK_l),		sendMessage $ Go R)
	, ((modm,			xK_h),		sendMessage $ Go L)
	, ((modm,			xK_k),		do; wsname <- gets (currentTag . windowset); if wsname == "jabber" then windows focusUp else sendMessage $ Go U)
	, ((modm,			xK_j),		do; wsname <- gets (currentTag . windowset); if wsname == "jabber" then windows focusDown else sendMessage $ Go D)
	, ((modm .|. shiftMask,		xK_l),		sendMessage $ Swap R)
	, ((modm .|. shiftMask,		xK_h),		sendMessage $ Swap L)
	, ((modm .|. shiftMask,		xK_k),		do; wsname <- gets (currentTag . windowset); if wsname == "jabber" then windows swapUp else sendMessage $ Swap U)
	, ((modm .|. shiftMask,		xK_j),		do; wsname <- gets (currentTag . windowset); if wsname == "jabber" then windows swapDown else sendMessage $ Swap D)
	, ((modm,			xK_p),		shellPrompt myXPConfig)
	, ((modm .|. shiftMask,		xK_f),		XS.modify (\(FuckStatus (x, y)) -> FuckStatus (not x, y)))
	, ((modm,			xK_a),		scratchpadSpawnActionCustom "urxvt -name scratchpad -e ghci")
	, ((modm .|. shiftMask,		xK_s),		namedScratchpadAction scratchpads "vimscratch")
	--, ((modm, xK_g), goToSelected defaultGSConfig)
	] ++
	-- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
	-- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
	[((m .|. modm,			key),		screenWorkspace sc >>= flip whenJust (windows . f))
		| (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
		, (f, m) <- [(W.view, 0), (W.shift, shiftMask)]] ++
--{{{ Workspace and tab jumping bindings
	[((m .|. modm,			k),		windows $ f i)
		| (i, k) <- zip (XMonad.workspaces conf) $ xK_grave : [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal, xK_backslash, xK_BackSpace] ++ [xK_F1 .. xK_F12]
		, (f, m) <- [(greedyView, 0), (shift, shiftMask)]] ++
	[((modm .|. mod1Mask,		k),		tabJump i) | (i, k) <- zip [0..9] [xK_0 .. xK_9]]
--}}}
--}}}
--{{{ mkTerm
mkTerm = withWindowSet launchTerminal

launchTerminal ws = case peek ws of
	Nothing -> runInTerm "" "exec $SHELL"
	Just xid -> terminalInCwd xid

terminalInCwd xid = let
		hex = showHex xid " "
		shInCwd = "'cd $(readlink /proc/$(ps --ppid $(xprop -id 0x" ++ hex ++ "_NET_WM_PID | cut -d\" \" -f3) -o pid= | tr -d \" \")/cwd) && $SHELL'"
	in runInTerm "" $ "sh -c " ++ shInCwd
--}}}
--{{{ Tab jumper
curstack :: W.StackSet i l a sid sd -> W.Stack a
curstack s = fromJust $ W.stack $ W.workspace $ W.current s

tabJump :: Int -> X ()
tabJump x = withWindowSet $ (\c -> focusNth $ if c `mod` 10 == x then c + 10 else x) . focused
	where focused = (\(Stack _ ls _) -> length ls) . curstack
--}}}
--{{{ Mouse bindings 
myMouseBindings :: (XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ()))
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
	-- mod-button1, Set the window to floating mode and move by dragging
	[ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows shiftMaster))
	-- mod-button2, Raise the window to the top of the stack
	, ((modm, button2), (\w -> focus w >> windows shiftMaster))
	-- mod-button3, Set the window to floating mode and resize by dragging
	, ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows shiftMaster))
	]
--}}}
--{{{ Layouts
myLayout = ModifiedLayout xxkb $ layoutHintsToCenter $ cn $ smartBorders $
	--onWorkspace "web" (full ||| grid) $
	onWorkspace "web" (full ||| vtab) $
--	onWorkspace "jabber" (tabBar shrinkText myTheme Bottom $ withIM (10%65) (ClassName "Tkabber") full) $
	--onWorkspace "jabber" ((im full) ||| grid) $
	onWorkspace "jabber" (im htab ||| full) $
	onWorkspace "stuff" (grid ||| full) $
	onWorkspace "status" (Mirror $ withIM (2%100) (Title "dzencontent") (Tall 1 (1/100) (70/100)) ||| full) $
	onWorkspace "backspace" full $
	--onWorkspace "backspace" (simpleFloat ||| tiles) $
	tiles ||| htab
	where
		--nodumbborders = resizeHorizontal n . resizeVertical n . resizeHorizontalRight n . resizeVerticalBottom n
		--	where n = -1
		nodumbborders = id
		cn = configurableNavigation (navigateColor "#00FF00") 
		dwmify = DWM.dwmStyle DWM.shrinkText myTheme 
		--tiled = decoration shrinkText myTheme DefaultDecoration $ Tall 2 (3/100) (6/10) 
		--tiled = tallSimpleTabbed
		grid = nodumbborders $ dwmify $ GridRatio (4/3)
		tiled = nodumbborders $ Tall 2 (3/100) (54/100)	-- 80 columns on the both sides at 1400px
		--htab = noBorders $ reflectHoriz $ tabbedBottomAlways shrinkText myTheme
		htab = noBorders $ reflectHoriz $ mtabbed 5 myName MT.shrinkText myTheme
		vtab = noBorders $ reflectHoriz $ vtabbed 200 myName VT.shrinkText myTheme
		--tabbed = noBorders $ reflectHoriz $ tabBar shrinkText myTheme Bottom Full
		full = noBorders Full
		im = withIM (10%65) (ClassName "Tkabber" `Or` (Resource "main" `And` ClassName "psi") `Or` Role "roster")
		--tiles = (dwmify $ tiled) ||| (dwmify $ Mirror tiled)
		--tiles = dwmify $ tiled
		tiles = tiled
--}}}
--{{{ Window rules
xxkb = monitor
	{ prop = ClassName "XXkb"
	, rect = Rectangle (1920-13) (1080-13) 13 13
	, persistent = True
	}

myManageHook = composeOne [ 
	className =? "Gimp" -?> doFloat,
	className =? "evilrun" -?> doRectFloat (RationalRect 0 0 1 (1%10)),
	className =? "Wine" -?> idHook,

	className =? "Chat" -?> moveTo "jabber",
	className =? "Tkabber" -?> moveTo "jabber",
	className =? "Gajim" -?> moveTo "jabber",
	className =? "Message" -?> moveTo "jabber",
	className =? "ErrorDialog" -?> moveTo "jabber",
	className =? "XData" -?> moveTo "jabber",
	--className =? "Dialog" -?> moveTo "jabber",	-- makes tkabber errors producing infinite focus-switching loops
	className =? "psi" -?> moveTo "jabber",
	className =? "Psi-plus" -?> moveTo "jabber",
	className =? "Uzbl-core" -?> moveTo "web",
	className =? "Firefox" -?> moveTo "web",
	className =? "Midori" -?> moveTo "web",
	className =? "Dillo" -?> moveTo "web",
	className =? "dwb" -?> moveTo "web",
	className =? "Dwb" -?> moveTo "web",
	className =? "qutebrowser" -?> moveTo "web",
	className =? "Skype" -?> moveTo "stuff",
	className =? "Googleearth-bin" -?> moveTo "stuff",
	className =? "Marble Virtual Globe" -?> moveTo "stuff",
	className =? "openttd" -?> moveTo "stuff",
	className =? "Mumble" -?> moveTo "stuff",
	title =? "rtorrent" -?> moveTo "stuff",
	className =? "Bitcoin" -?> moveTo "stuff",
	className =? "MPlayer" -?> moveTo "stuff",
	className =? "mplayer2" -?> moveTo "stuff",
	className =? "mpv" -?> moveTo "stuff",
	className =? "Claws-mail" -?> moveTo "stuff",
	className =? "Ossxmix" -?> moveTo "stuff",
	className =? "Transmission-gtk" -?> moveTo "stuff",
	className =? "Transmission-qt" -?> moveTo "stuff",
	className =? "transmission" -?> moveTo "stuff",
	className =? "Blink" -?> moveTo "stuff",
	className =? "Linphone" -?> moveTo "stuff",
	className =? "Apvlv" -?> moveTo "reading",
	className =? "XDvi" -?> moveTo "reading",
	className =? "Epdfview" -?> moveTo "reading",
	className =? "MuPDF" -?> moveTo "reading",
	className =? "llpp" -?> moveTo "reading",
	className =? "Zathura" -?> moveTo "reading",
	className =? "Fbreader" -?> moveTo "reading",
	title =? "ncmpcpp" -?> moveTo "status",
	className =? "Conky" -?> moveTo "status",
	title =? "dzencontent" -?> moveTo "status",
	title =? "atop" -?> moveTo "status",
	title =? "pinger" -?> moveTo "status",
	title =? "syslog" -?> moveTo "status",

	return True -?> namedScratchpadManageHook scratchpads >> doSink
	]
	--isFullscreen --> doFullFloat]
	where	moveTo = doF . shift
		doSink = ask >>= \w -> liftX (reveal w) >> doF (W.sink w)
--}}}
--{{{ Fuck firefox
data FuckStatus = FuckStatus (Bool, Bool) deriving (Typeable,Read,Show)

instance ExtensionClass FuckStatus where
	initialValue = FuckStatus (True, False)
	extensionType = PersistentExtension

myLogHook = do
	FuckStatus (enabled, onws) <- XS.get
	when enabled $ do
		wsname <- gets (currentTag . windowset)
		case wsname of
			"web" -> when (not onws) $ do
				fuckFirefox False
				XS.put $ FuckStatus (enabled, True)
			"" -> return ()
			_ -> when onws $ do
				fuckFirefox True
				XS.put $ FuckStatus (enabled, False)

fuckFirefox ye = do
	liftIO $ forkIO $ do
		-- grsec :/
		(rc, out, _) <- readProcessWithExitCode' "sudo" ["pgrep", "firefox"] []
		(rc, out2, _) <- readProcessWithExitCode' "sudo" ["pgrep", "qutebrowser"] []
		mapM_ fuckIt ((lines $ out) ++ (lines $ out2))
	return ()
	where fuckIt s = signalProcess (if ye then sigSTOP else sigCONT) (CPid $ read s)

readProcessWithExitCode'
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readProcessWithExitCode' cmd args input = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }

    outMVar <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    out  <- hGetContents outh
    forkIO $ C.evaluate (length out) >> putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    err  <- hGetContents errh
    forkIO $ C.evaluate (length err) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- C.catch (waitForProcess pid >>= return) (\e -> seq (e :: C.SomeException) $ return $ ExitSuccess)

    return (ex, out, err)
--}}}
--{{{ Main config
main = let conf = ignoreNetActiveWindow (return True) $ withUrgencyHookC (\w -> do
			borderUrgencyHook "#ffff00" w
			spawnUrgencyHook "urge " w)
			(UrgencyConfig {
				suppressWhen = Focused,
				remindWhen = Every 60
			}) $
		defaultConfig
			{ focusFollowsMouse  = False
			, borderWidth        = 1
			, modMask            = mod4Mask
			, workspaces         = ["status","root","web","jabber","user","stuff","ssh","reading","8","9","0","-","=","\\","backspace", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12"]
			, normalBorderColor  = "#999999"
			, focusedBorderColor = "#FF0000"
			, keys               = myKeys
			, mouseBindings      = myMouseBindings
			, layoutHook         = myLayout
			, manageHook         = myManageHook <+> manageMonitor xxkb
			, logHook            = myLogHook
			, handleEventHook    = perWindowKbdLayout <+> ewmhDesktopsEventHook	-- full ewmh disabled due to https://github.com/xmonad/xmonad-contrib/issues/227
			, startupHook        = disableAutoRepeat >> setWMName "LG3D" >> ewmhDesktopsStartup
		} in
	xmonad $ conf
--}}}
-- vim: foldmethod=marker
