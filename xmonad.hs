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
import XMonad.Util.NamedWindows
import XMonad.Util.Scratchpad

-- Local libraries
import XMonad.Hooks.PerWindowKbdLayout
import XMonad.Hooks.DisableAutoRepeat
import XMonad.Layout.VTabbed as VT
import XMonad.Layout.MTabbed as MT
--}}}
--{{{ Namer

instance Namer CustomNamer where
    nameIt _ w = do
                    ws <- gets windowset
                    nw <- getName w
                    let num = maybe "" (\x -> (show $ x + 1) ++ ":") $ elemIndex w (W.integrate' $ W.stack
                                                                          $ W.workspace $ W.current ws)
                    return $ num ++ (show nw)

myName = CustomNamer
--}}}
--{{{ Theme
myTheme = defaultTheme {
	fontName = "xft:Terminus:size=14",
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
--{{{ Key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
	[ ((modm .|. shiftMask,		xK_c),		kill)
	, ((modm,			xK_space),	sendMessage NextLayout)
	, ((modm .|. shiftMask,		xK_space),	setLayout $ XMonad.layoutHook conf)
	, ((modm,			xK_n),		refresh)
	, ((modm,			xK_m),		windows focusMaster)
	, ((modm,			xK_Return),	windows swapMaster)
	, ((modm .|. controlMask,	xK_h),		sendMessage Shrink)
	, ((modm .|. controlMask,	xK_l),		sendMessage Expand)
	, ((modm,			xK_t),		withFocused $ windows . sink)
	, ((modm,			xK_comma),	sendMessage (IncMasterN 1))
	, ((modm,			xK_period),	sendMessage (IncMasterN (-1)))
	, ((modm .|. shiftMask,		xK_q),		io (exitWith ExitSuccess))
	, ((modm,			xK_u),		focusUrgent)
	, ((modm,			xK_g),		windowPromptGoto myXPConfig)
	-- Leave the vanilla focusing bindings to work with Full
	, ((modm,			xK_Down),	windows focusDown)
	, ((modm,			xK_Up),		windows focusUp)
	, ((modm,			xK_l),		sendMessage $ Go R)
	, ((modm,			xK_h),		sendMessage $ Go L)
	, ((modm,			xK_k),		sendMessage $ Go U)
	, ((modm,			xK_j),		sendMessage $ Go D)
	, ((modm .|. shiftMask,		xK_j),		windows swapDown)
	, ((modm .|. shiftMask,		xK_k),		windows swapUp)
	, ((modm .|. shiftMask,		xK_l),		sendMessage $ Swap R)
	, ((modm .|. shiftMask,		xK_h),		sendMessage $ Swap L)
	, ((modm .|. shiftMask,		xK_k),		sendMessage $ Swap U)
	, ((modm .|. shiftMask,		xK_j),		sendMessage $ Swap D)
	, ((modm,			xK_p),		shellPrompt myXPConfig)
	, ((modm .|. shiftMask,		xK_f),		XS.modify (\(FuckStatus (x, y)) -> FuckStatus (not x, y)))
	, ((modm,			xK_a),		scratchpadSpawnActionCustom "urxvt -name scratchpad -e ghci")
	--, ((modm, xK_g), goToSelected defaultGSConfig)
	] ++
	-- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
	-- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
	[((m .|. modm,			key),		screenWorkspace sc >>= flip whenJust (windows . f))
		| (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
		, (f, m) <- [(W.view, 0), (W.shift, shiftMask)]] ++
--{{{ Workspace and tab jumping bindings
	[((m .|. modm,			k),		windows $ f i)
		| (i, k) <- zip (XMonad.workspaces conf) $ xK_grave : [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal, xK_backslash, xK_BackSpace]
		, (f, m) <- [(greedyView, 0), (shift, shiftMask)]] ++
	[((modm .|. mod1Mask,		k),		tabJump i) | (i, k) <- zip [0..9] [xK_0 .. xK_9]]
--}}}
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
myLayout = layoutHintsToCenter $ cn $ smartBorders $
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
		im = withIM (10%65) (ClassName "Tkabber" `Or` (Resource "main" `And` ClassName "psi"))
		--tiles = (dwmify $ tiled) ||| (dwmify $ Mirror tiled)
		--tiles = dwmify $ tiled
		tiles = tiled
--}}}
--{{{ Window rules
myManageHook = composeOne [ 
	className =? "Gimp" -?> doFloat,
	className =? "evilrun" -?> doRectFloat (RationalRect 0 0 1 (1%10)),
	className =? "Wine" -?> idHook,

	className =? "Chat" -?> moveTo "jabber",
	className =? "Tkabber" -?> moveTo "jabber",
	className =? "Message" -?> moveTo "jabber",
	className =? "ErrorDialog" -?> moveTo "jabber",
	className =? "XData" -?> moveTo "jabber",
	className =? "Dialog" -?> moveTo "jabber",
	className =? "psi" -?> moveTo "jabber",
	className =? "Psi-plus" -?> moveTo "jabber",
	className =? "Uzbl-core" -?> moveTo "web",
	className =? "Firefox" -?> moveTo "web",
	className =? "Midori" -?> moveTo "web",
	className =? "Dillo" -?> moveTo "web",
	className =? "dwb" -?> moveTo "web",
	className =? "Dwb" -?> moveTo "web",
	className =? "Skype" -?> moveTo "stuff",
	className =? "Googleearth-bin" -?> moveTo "stuff",
	className =? "Marble-qt" -?> moveTo "stuff",
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

	return True -?> doSink
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
		liftIO $ putStrLn wsname
		case wsname of
			"web" -> when (not onws) $ do
				liftIO $ putStrLn "unfucking"
				fuckFirefox False
				XS.put $ FuckStatus (enabled, True)
			"" -> return ()
			_ -> when onws $ do
				liftIO $ putStrLn "fucking"
				fuckFirefox True
				XS.put $ FuckStatus (enabled, False)

fuckFirefox ye = do
	liftIO $ forkIO $ do
		(rc, out, _) <- readProcessWithExitCode' "pgrep" ["firefox"] []
		--when ye $ threadDelay 50000
		mapM_ fuckIt $ lines $ out
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
main = let conf = ewmh $ withUrgencyHookC (\w -> do
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
			, workspaces         = ["status","root","web","jabber","user","stuff","ssh","reading","8","9","0","-","=","\\","backspace"]
			, normalBorderColor  = "#999999"
			, focusedBorderColor = "#FF0000"
			, keys               = myKeys
			, mouseBindings      = myMouseBindings
			, layoutHook         = myLayout
			, manageHook         = myManageHook
			, handleEventHook    = perWindowKbdLayout
			, logHook            = myLogHook
			, startupHook        = disableAutoRepeat
		} in
	xmonad $ conf { startupHook = startupHook conf >> setWMName "LG3D" }
--}}}
-- vim: foldmethod=marker
