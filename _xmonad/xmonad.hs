{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-deprecations -Wno-overflowed-literals #-}
import           XMonad as X                         hiding ((|||))
import qualified XMonad.StackSet as W
import           XMonad.ManageHook
import           XMonad.Prompt
import           XMonad.Prompt.Man
import           XMonad.Prompt.Workspace
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.DynamicHooks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.DynamicBars
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Hooks.Minimize
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.WindowSwallowing
import           XMonad.Layout.Accordion
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Grid
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
-- import           XMonad.Layout.TwoPane
import           XMonad.Layout.TwoPanePersistent
import           XMonad.Layout.ResizableTile     -- Resizable Horizontal border
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.StackTile
import           XMonad.Layout.Reflect
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.Spacing           -- this makes smart space around windows
import           XMonad.Layout.AutoMaster
import           XMonad.Layout.WindowNavigation
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Util.EZConfig
import           XMonad.Util.Run
import           XMonad.Util.Dmenu
import           XMonad.Util.Minimize
import           XMonad.Util.NamedWindows (getName)
-- import           XMonad.Util.Scratchpad
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.SpawnOnce
-- import           XMonad.Util.WorkspaceCompare
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Actions.AfterDrag
import           XMonad.Actions.FindEmptyWorkspace
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.CopyWindow(copy)
import           XMonad.Actions.WindowBringer
import           XMonad.Actions.GridSelect
import           XMonad.Actions.WithAll
import           XMonad.Actions.FloatKeys
import           XMonad.Actions.WorkspaceNames
import           XMonad.Actions.RotSlaves
-- import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.UpdateFocus
import           XMonad.Actions.Warp
import           XMonad.Actions.GroupNavigation
import           XMonad.Actions.SwapPromote
import           XMonad.Actions.Minimize
import qualified XMonad.Actions.SwapWorkspaces as SWS
import           XMonad.Actions.Submap
import           XMonad.Actions.WindowGo
import           Graphics.X11.ExtraTypes.XF86
import           System.IO
import           System.Exit

--import           System.Directory
--import           System.Process
--import qualified Control.Exception.Extensible as E
--import           XMonad.Prompt.FuzzyMatch

import           Data.Maybe
import           Data.Char
import           Data.Foldable
import qualified Data.Map as M
import           Data.List
import           Data.List.Split
import           Control.Monad

defaultSpacing = 0

dchoice :: [String] -> [String] -> [X()] -> X()
dchoice args items actions = do
  result <- XMonad.Util.Dmenu.menuArgs "dmenu" args items
  when (True) (snd $ head [ element | element <- zip items actions, fst element == result ])

-- dconfirm args items action = do
--   result <- XMonad.Util.Dmenu.menuArgs "dmenu" args items
--   when (result == last items) action

-- | Testing. Move active window to master area. Purpose: bring focused window to top with floating layout. Not working now
promoteToMaster :: X()
promoteToMaster = windows $ W.modify' promoteToMaster'

promoteToMaster' :: W.Stack a -> W.Stack a
promoteToMaster' (W.Stack t l r) = W.Stack t' l' r'
  where rvlr = reverse l ++ r
        (l',t',r')=([t], head rvlr,drop 1 rvlr)

-- | Move active window to the top of slave stack. Will be replaced by XMonad.Layout.TwoPanePersistent from xmonad-contrib 0.16
promoteSlave :: X()
promoteSlave = windows $ W.modify' promoteSlave'

promoteSlave' :: W.Stack a -> W.Stack a
promoteSlave' (W.Stack t l r) = W.Stack t' l' r'
  where rvlr = reverse l ++ r
        (l',t',r')=(take 1 rvlr,t,drop 1 rvlr)

barCreator :: DynamicStatusBar
barCreator (X.S sid) = spawnPipe $ "/usr/bin/xmobar --screen " ++ show sid ++ " /home/darek/.xmonad/xmobarrc"

barDestroyer :: DynamicStatusBarCleanup
barDestroyer = return ()

-- | Count the number of windows on workspace
winCount :: X String
--winCount = do
--   ws' <- length . W.integrate' . W.stack . W.workspace . W.current . windowset <$> get
--   return $ show ws'
-- winCount = show . length . W.integrate' . W.stack . W.workspace . W.current . windowset <$> get
-- winCount = show . length . W.index . windowset <$> get
winCount = gets (windowset) >>= return . show . length . W.index

wsCount :: X String
wsCount = gets (namedScratchpadFilterOutWorkspace . W.workspaces . windowset) >>= return . show . length

--workspaceCount :: X String
--workspaceCount = withWindowSet $ \ws -> do return . show . length $ W.hidden ws ++ map W.workspace (W.current ws : W.visible ws)
--workspaceCount = withWindowSet $ \ws -> do
--    let wss = reverse $ map W.tag $ W.hidden ws ++ map W.workspace (W.current ws : W.visible ws)
--wsCount = gets (W.workspaces . windowset) >>= return . show . length
--wsCount = gets winCount >>= return . show . length

getWindowCounts :: X (WorkspaceId -> String)
getWindowCounts = do
   -- workspaces <- gets (W.workspaces . windowset)
   minimizedWindows <- XS.gets minimizedStack
   minimizedCounts <- gets (map (length . (intersect minimizedWindows). W.integrate' . W.stack) . W.workspaces . windowset)
   let minimizedPresent = map (\x -> if x > 0 then (-1) else 1) minimizedCounts
   windowCounts <- gets (map (length . W.integrate' . W.stack) . W.workspaces . windowset)
   tags <- gets (map (W.tag) . W.workspaces . windowset)
   let wsToWinCount = zip tags $ zipWith (*) windowCounts minimizedPresent
   let getCount = \w -> fromMaybe 0 $ lookup w wsToWinCount
   return $ \wks -> pp (getCount $ head $ splitOn ":" (trim wks)) ++ wks -- splitOn - accomodate for :NAME behind WorkspaceId
      where pp :: Int -> String
            pp c | c == 0     = ""
                 | abs(c) == 1     = select c "¹" "₁"
                 | abs(c) == 2     = select c "²" "₂"
                 | abs(c) == 3     = select c "³" "₃"
                 | abs(c) == 4     = select c "⁴" "₄"
                 | abs(c) == 5     = select c "⁵" "₅"
                 | abs(c) == 6     = select c "⁶" "₆"
                 | abs(c) == 7     = select c "⁷" "₇"
                 | abs(c) == 8     = select c "⁸" "₈"
                 | abs(c) == 9     = select c "⁹" "₉"
                 | otherwise       = select c "⁺" "₊"
            select :: Int -> String -> String -> String
            select i c1 c2 = if i > 0 then c1 else c2

windowCountsPP :: PP -> X PP
windowCountsPP pp = do
    counts <- getWindowCounts
    names <- getWorkspaceNames'
    return $
        pp {
              ppCurrent         = ppCurrent         pp . counts
            , ppVisible         = ppVisible         pp . counts
            , ppHidden          = ppHidden          pp . counts
            , ppHiddenNoWindows = ppHiddenNoWindows pp . counts
            , ppUrgent          = ppUrgent          pp . counts
        }

mygetWorkspaceNames :: X (WorkspaceId -> String)
mygetWorkspaceNames = do
    lookup <- getWorkspaceNames'
    return $ \wks -> wks ++ maybe "" (':' :) (lookup wks)

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- | Testing: for layout without xmobar -> display briefly screen name
curWS :: X String
curWS = do
   tag <- gets (W.currentTag . windowset)
   name <- curWSName
   let decorated_name = if name == "" then "" else " : " ++ map toUpper name
   return $ tag ++ decorated_name

curWSName :: X String
curWSName = getCurrentWorkspaceName >>= return . fromMaybe ""

-- | Testing: get layout parameters
getLayout :: X String
getLayout = do
   layout <- gets (description . W.layout . W.workspace . W.current . windowset)
   return $ show layout

-- | Prompt for a new name for the current workspace and set it.
wsPromptXPConfig :: XPConfig
wsPromptXPConfig = def {
           font = "xft:DejaVuSansMono:pixelsize=10:antialias=true:hinting=true"
           , bgColor = "black"
           , fgColor = "gray"
           , borderColor = "red"
           , promptBorderWidth = 0
           , height = 18
           , historySize = 0
           }

renameWorkspace' :: XPConfig -> X ()
renameWorkspace' conf = do
    --mkXPrompt pr conf (const (return ["MAIL","NOTES","WEBEX","FILES","SYS","WWW","DEV","VM","DOC"])) setCurrentWorkspaceName
    mkXPrompt pr conf (const (return ["MAIL","NOTES","WEBEX","DEV","WWW", "VM", "TEAMS"])) setCurrentWorkspaceName
    where pr = Wor "workspace name: "

renameWS :: X ()
renameWS = do
    renameWorkspace' wsPromptXPConfig 

-- | jump to workspace (ignores passed config)
-- wsPrompt :: XPConfig -> (String -> X ()) -> X ()
-- wsPrompt conf job = do 
--     myWorkspaces <- gets $ map W.tag . W.workspaces . windowset
--     myWorkspacesName <- getWorkspaceNames >>= \f -> return $ map (lowCase . f) myWorkspaces
--     let pairs = zip myWorkspacesName myWorkspaces
--     -- mkXPrompt (Wor "Select workspace: ") wsPromptXPConfig { autoComplete = Just 10000 }
--     mkXPrompt (Wor "Select workspace: ") wsPromptXPConfig
--               (contains myWorkspacesName)
--               (job . toWsId pairs)
--   where toWsId pairs name = case lookup name pairs of
--                                 Nothing -> ""
--                                 Just i -> i
--         contains completions input =
--             return $ filter (Data.List.isInfixOf input) completions

-- | jump to layout (ignores passed config, for now list of layouts needs to be statically defined)
-- layoutPrompt :: XPConfig -> X ()
-- layoutPrompt c = do 
--     -- ls <- gets (map (description . W.layout) . W.workspaces . windowset)
--     let ls = myLayouts
--     mkXPrompt (Wor "") wsPromptXPConfig { autoComplete = Just 10000 } (mkComplFunFromList' $ sort $ nub ls) (sendMessage . JumpToLayout)

-- | Open file manager in the directory containing selected file
-- topPromptXPConfig :: XPConfig
-- topPromptXPConfig = def {
--                        font = "xft:DejaVuSansMono:pixelsize=10:antialias=true:hinting=true"
--                        , bgColor = "black"
--                        , fgColor = "gray"
--                        , borderColor = "red"
--                        , promptBorderWidth = 0
--                        , height = 18
--                        , alwaysHighlight = True
--                        , position = Top
--                        , maxComplRows = Just 10
--                        , historySize = 0
--                     }
-- getFiles :: IO [String]
-- getFiles = do
--     let getout cmd = getCommandOutput cmd `E.catch` \E.SomeException{} -> return ""
--     files <- getout "find ~/WORK/ -type f > /tmp/xmonad.find && cat /tmp/xmonad.find"
--     return $ lines $ files
-- 
-- type Predicate = String -> String -> Bool
-- 
-- getFileCompl :: [String] -> Predicate -> String -> IO [String]
-- getFileCompl compls p s | s == "" || last s == ' '  = return []
--                         | otherwise                 = return $ fuzzySort s $ filter (p s) compls

-- gotoDirPrompt :: XPConfig -> X ()
-- gotoDirPrompt conf = do
--     files <- io getFiles
--     mkXPrompt (Wor "Select file: ") topPromptXPConfig (getFileCompl files $ fuzzyMatch) $ runInTerm "" . (++) "nnn " 

-- | Add workspace names (and later list or count of windows) to gridselectWorkspace
mygridselectWorkspace :: GSConfig WorkspaceId ->
                          (WorkspaceId -> WindowSet -> WindowSet) -> X ()
mygridselectWorkspace conf viewFunc = mygridselectWorkspace' conf (windows . viewFunc)

-- | Select a workspace and run an arbitrary action on it.
mygridselectWorkspace' :: GSConfig WorkspaceId -> (WorkspaceId -> X ()) -> X ()
mygridselectWorkspace' conf func = withWindowSet $ \ws -> do
    -- let wss = reverse $ map W.tag $ W.hidden ws ++ map W.workspace (W.current ws : W.visible ws)
    -- wss <- return (reverse $ map W.tag $ W.hidden ws ++ map W.workspace (W.current ws : W.visible ws))
    -- spawn $ "notify-send " ++ show wss
    -- counts <- getWindowCounts
    -- names <- getWorkspaceNames
    myWorkspaces <- gets $ map W.tag . W.workspaces . windowset
    -- myWorkspacesName <- getWorkspaceNames >>= \f -> return $ map (upCase . f) myWorkspaces
    myWorkspacesName <- mygetWorkspaceNames >>= \f -> return $ map (upCase . f) myWorkspaces
    -- spawn $ "notify-send " ++ show myWorkspacesName
    -- spawn $ "notify-send " ++ show myWorkspaces
    -- spawn $ "notify-send " ++ show wss
    let pairs = zip myWorkspacesName myWorkspaces
    -- gridselect conf (zip wss wss) >>= flip whenJust func
    -- gridselect conf pairs >>= flip whenJust func
    gridselect conf pairs >>= flip whenJust func

-- | Add workspace names and windows counts to log string
myLogPP :: PP -> X String
myLogPP pp = do
    -- works correctly in this order: windowCountsPP takes into account workspaceNamesPP that adds :NAME decoration
    windowCountsPP pp >>= workspaceNamesPP >>= dynamicLogString  
    -- windowCountsPP pp >>= dynamicLogString
    -- workspaceNamesPP pp >>= windowCountsPP >>= dynamicLogString

-- | Execute action when clicking on ws name in xmobar
myXmobarAction ws = xmobarAction ("xdotool key alt+" ++ wsid) "1" ws
    where wsid = head $ splitOn ":" (takeWhile (isDigit) $ dropWhile (not . isDigit) $ trim ws)

-- | hide all windows from current jspace
hideWindows :: X ()
hideWindows = withWindowSet $ \ws -> traverse_ minimizeWindow (W.index ws)

-- | restore all windows from current workspace
restoreWindows :: X ()
restoreWindows = withMinimized $ \ws -> traverse_ maximizeWindowAndFocus ws

-- | helper functions
upCase :: WorkspaceId -> String
upCase tag = map toUpper tag

lowCase :: WorkspaceId -> String
lowCase tag = map toLower tag

-- | dock layout (like TwoPane but allowing for increasing the number of windows in master area)
data NewDock a = NewDock !Int !Rational !Rational deriving (Show, Read)

instance LayoutClass NewDock a where
    pureLayout (NewDock nmaster _ frac) r s = zip ws rs
      where ws = (take nmaster $ reverse (W.up s)) ++ [W.focus s] ++ (drop nmaster $ reverse (W.up s)) ++ W.down s
            rs = dish frac r nmaster (length ws)

    pureMessage (NewDock nmaster delta frac) m = 
            msum [fmap resize     (fromMessage m)
                 ,fmap incmastern (fromMessage m)]
      where resize Shrink             = NewDock nmaster delta (max 0 $ frac-delta)
            resize Expand             = NewDock nmaster delta (min 1 $ frac+delta)
            incmastern (IncMasterN d) = NewDock (max 0 (nmaster+d)) delta frac
    description _ = "NewDock"

dish :: Rational -> Rectangle -> Int -> Int -> [Rectangle]
dish f r nmaster n = if n <= nmaster || nmaster == 0
    then splitVertically n r
    else splitVertically nmaster r1 ++ [r2] -- two columns
  where (r1,r2) = splitHorizontallyBy f r

-- Window rules
-- | Execute arbitrary actions and WindowSet manipulations when managing
-- a new window.
myManageHook = [ 
                className =? "firefox" --> doShift "3"
                , appName =? "joplin" --> doShift "8"
                -- , title =? "Mozilla Firefox" --> doShift "3"
                , appName =? "Mail" --> doShift "9"
                , isInProperty "WM_NAME" "Calendar" --> doShift "9"
                -- , title =? "Calendar" --> doShift "9"
                -- , className =? "Firefox" --> doF W.focusDown
            ]

-- | Main program
main = do
  -- | for signle monitor status bar
  -- statusBar <- spawnPipe "/usr/bin/xmobar /home/darek/.xmonad/xmobarrc"
  xmonad $ docks $ withUrgencyHook NoUrgencyHook $ ewmh def {
    modMask = modMask
    , borderWidth = 1
    , normalBorderColor = "#404040"
    , focusedBorderColor = "#FF0000"
    , terminal = "alacritty"
    , manageHook = composeAll myManageHook <+> composeAll [
               manageDocks
               , namedScratchpadManageHook $ myScratchpads
               -- , scratchpadManageHook $ (W.RationalRect 0.2 0.2 0.6 0.5)
               -- , scratchpadManageHookDefault
               , dynamicMasterHook
               , manageHook def
               ]
    , layoutHook = mkToggle (single REFLECTX) $
                   mkToggle (single REFLECTY) $
                   avoidStruts $ 
                   tall ||| wide ||| pdock ||| ndock ||| full ||| three ||| grid ||| acc ||| stack ||| autom ||| flt
    , handleEventHook = mconcat [
                          docksEventHook
                          , minimizeEventHook
                          , focusOnMouseMove
                          , handleEventHook def <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook <+> swallowEventHook (className =? "Alacritty" <||> className =? "st-256color") (return True)
                          -- , handleEventHook def <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook
                          , dynStatusBarEventHook barCreator barDestroyer 
                        ]
    , workspaces = myWorkspaces
    , logHook = multiPPFormat myLogPP (def {
                           ppCurrent = xmobarColor "white" "#005577" . wrap "[" "]" . upCase
                           , ppVisible = xmobarColor "darkorange" "" . wrap "<" ">" . myXmobarAction . upCase     -- active on another monitor
                           , ppHiddenNoWindows = pad . myXmobarAction . upCase      -- not visible and no windows
                           , ppHidden = pad . myXmobarAction . upCase               -- not visible but with windows
                           , ppUrgent  = xmobarColor "red" "gray" . pad . myXmobarAction . upCase
                           , ppSep = " | "
                           , ppWsSep = " "
                           , ppTitle = xmobarColor "white" "#005577" . shorten 80 . wrap " " (replicate 80 ' ')
                           -- , ppSort = fmap (. scratchpadFilterOutWorkspace) $ getSortByXineramaRule     -- group by screens
                           , ppSort = fmap (. take 9 . namedScratchpadFilterOutWorkspace) $ ppSort def     -- hide scratchpad from workspace list; show only 10 workspaces
                           -- , ppOrder = \(ws:l:wt:_) -> [ws,l]  -- strip active window title
                           , ppLayout = id
                           }) (def {
                                   ppCurrent = wrap "[" "]" . upCase
                                   , ppVisible = wrap "<" ">" . myXmobarAction . upCase     -- active on another monitor; TODO: change to color only(?)
                                   , ppHiddenNoWindows = pad . myXmobarAction . upCase      -- not visible and no windows
                                   , ppHidden = pad . myXmobarAction . upCase               -- not visible but with windows
                                   , ppUrgent  = xmobarColor "red" "gray" . pad . myXmobarAction . upCase
                                   , ppSep = " | "
                                   , ppWsSep = " "
                                   , ppTitle = shorten 80 . wrap " " ""
                                   -- , ppSort = fmap (. scratchpadFilterOutWorkspace) $ getSortByXineramaRule   -- group by screens
                                   , ppSort = fmap (. namedScratchpadFilterOutWorkspace) $ ppSort def      -- hide scratchpad from workspace list
                                   -- , ppOrder = \(ws:l:wt:_) -> [ws,l]  -- strip active window title
                                   , ppLayout = id
                                   -- }) >> updatePointer (0.05, 0.05) (0.1, 0.1) >> historyHook >> masterHistoryHook
                                   }) >> historyHook >> masterHistoryHook
-- | for signle monitor status bar
--    , logHook = dynamicLogWithPP $ xmobarPP
--                        { ppOutput = hPutStrLn statusBar
--                           , ppVisible = wrap "<" ">" . myXmobarAction . upCase     -- active on another monitor
--                           , ppHiddenNoWindows = pad . myXmobarAction . upCase      -- not visible and no windows
--                           , ppHidden = pad . myXmobarAction . upCase      -- not visible but with windows
--                           , ppUrgent  = xmobarColor "red" "gray" . pad . myXmobarAction . upCase
--                           , ppSep = " | "
--                           , ppWsSep = " "
--                           , ppTitle = xmobarColor "green" "" . shorten 80
--                           -- , ppSort = getSortByXineramaRule     -- group by screens
--                           , ppSort = fmap (.scratchpadFilterOutWorkspace) $ ppSort defaultPP     -- hide scratchpad from workspace list
--                           -- , ppOrder = \(ws:l:wt:_) -> [ws,l]  -- strip active window title
--                           , ppLayout = id
--                        }
    , startupHook = do
          spawnOnce "nitrogen --restore"
          -- spawnOnce "wpc &"
          spawnOnce "tray"
          -- spawnOnce "sh -c export QT_SCALE_FACTOR=0.50"
          -- tymczasowo przyblokowane:
             -- spawnOnce "picom &"
             -- spawnOnce "conky -d -c /home/darek/.conkyrc-stat"
          spawnOnce "setxkbmap -option caps:escape"
          spawnOnce "xm"
          -- spawnOnce "xmodmap -e 'keycode 117=NoSymbol'"
          -- spawnOnce "xmodmap -e 'keycode 112=NoSymbol'"
          -- spawnOnce "xautolock -time 10 -locker slock"
          -- spawnOnce "synclient TapButton2=3 TapButton1=1"
          -- spawnOnce "synclient RightButtonAreaLeft=0 RightButtonAreaTop=0"
          spawnOnce "xset r rate 200 40"
          spawnOnce "xset -dpms"
          spawnOnce "xset s off"
          dynStatusBarStartup barCreator barDestroyer
          setWMName "LG3D"
          adjustEventInput
  } `additionalKeys` ([ 
                       -- ((0, xK_F1), manPrompt topPromptXPConfig)
                       -- , ((mod1Mask, xK_F1), manPrompt def)
                       -- , ((modMask, xK_grave), scratchpadSpawnActionCustom "st -n scratchpad -e /usr/local/bin/tmux-run")
                       ((modMask, xK_grave), namedScratchpadAction myScratchpads "scratchpad")
                       -- ((modMask, xK_grave), scratchpadSpawnActionCustom "alacritty --class scratchpad --title scratchpad -e /usr/local/bin/tmux-run")
                       , ((modMask .|. controlMask, xK_l), spawn "slock")
                       -- , ((modMask, xK_Return), spawn "st")
                       -- , ((modMask .|. shiftMask, xK_Return), spawn "terminator")
                       -- | hide and restore windows on current workspace
                       , ((modMask, xK_v), hideWindows)
                       , ((modMask .|. shiftMask, xK_v), restoreWindows)
                       -- | set particular layout
                       -- , ((modMask, xK_n), sendMessage (JumpToLayout "accordion"))
                       , ((modMask, xK_c), sendMessage (JumpToLayout "pdock"))
                       , ((modMask, xK_f), sendMessage (JumpToLayout "full"))
                       -- , ((modMask, xK_s), sendMessage (JumpToLayout "stack"))
                       , ((modMask, xK_d), sendMessage (JumpToLayout "wide"))
                       , ((modMask, xK_g), sendMessage (JumpToLayout "grid"))
                       , ((modMask, xK_t), sendMessage (JumpToLayout "three"))
                       -- , ((modMask, xK_t), sendMessage (JumpToLayout "float"))
                       -- | resize windows in slave stack
                       , ((modMask, xK_z), sendMessage MirrorShrink)
                       , ((modMask, xK_a), sendMessage MirrorExpand)
                       -- | toggle status bar "visibility"
                       , ((modMask, xK_b), sendMessage $ ToggleStruts)
                       -- | zoom current window without affecting the layout
                       , ((modMask, xK_m), withFocused (sendMessage . maximizeRestore))
                       -- | rename workspace
                       , ((mod4Mask .|. shiftMask, xK_r), renameWS)   -- TODO: will need to change for 3-monitor setup
                       -- | goto workspace with prompt
                       -- , ((mod4Mask, xK_r), wsPrompt def (X.windows . W.greedyView))   -- TODO: will need to change with 3-monitor setup

                       -- | begin: just for testing
                       -- , ((modMask, xK_x), curWS >>= \d->spawn $ "notify-send "++d)
                       -- , ((modMask, xK_x), winCount >>= \d->spawn $ "notify-send "++d)
                       -- , ((modMask, xK_x), getLayout >>= \d->spawn $ "notify-send "++d)
                       -- , ((modMask, xK_x), gotoDirPrompt def)
                       -- , ((modMask .|. shiftMask, xK_m     ), layoutPrompt wsPromptXPConfig)
                       -- , ((modMask, xK_x), addWorkspacePrompt wsPromptXPConfig )
                       , ((modMask, xK_x), submap . M.fromList $
                           [ ((0, xK_a),     wsCount >>= \c->addWorkspace $ show ((read c) + 1)) -- add n+1 workspace
                           -- [ ((0, xK_a),     wsCount >>= \c->addWorkspace c) -- add
                           , ((0, xK_d),     removeWorkspace)                -- remove
                           , ((0, xK_n),     wsCount >>= (\d->spawn $ "notify-send " ++ d) >> addWorkspacePrompt wsPromptXPConfig ) -- add named
                           , ((0, xK_Return),     wsCount >>= (\d->spawn $ "notify-send " ++ d) ) -- show count
                           ])
                       -- , ((modMask .|. controlMask, xK_x), wsCount >>= \c->addWorkspace c)
                       -- , ((modMask .|. shiftMask, xK_x), removeWorkspace)
                       -- , ((modMask, xK_x), wsCount >>= (\d->spawn $ "notify-send " ++ show ((read d) + 1)) >> addWorkspacePrompt wsPromptXPConfig )
                       -- , ((modMask .|. controlMask, xK_x), addWorkspacePrompt wsPromptXPConfig )
                       -- , ((mod1Mask, xK_x), withFocused $ mouseResizeWindow) -- testing if in floating layout I can bring arbitrary/focused window to front
                       -- | end: just for testing

                       -- | find and view empty workspace
                       , ((modMask,                xK_y    ), viewEmptyWorkspace)
                       -- | move window to empty workspace and view it
                       , ((modMask .|. shiftMask,  xK_y    ), tagToEmptyWorkspace)
                       -- | promote window to master area
                       , ((modMask, xK_Return), whenX (swapHybrid False) (windows $ W.swapMaster))
                       , ((modMask, xK_slash), focusUrgent)
                       -- | mirror layout like spectrwm       
                       , ((modMask .|. shiftMask, xK_backslash), sendMessage $ Toggle REFLECTY)
                       , ((modMask, xK_backslash), sendMessage $ Toggle REFLECTX)
                       -- | float window
                       , ((modMask .|. shiftMask, xK_f), withFocused float)
                       -- | unfloat all windows
                       , ((modMask .|. shiftMask, xK_t), sinkAll)
                       -- | inc/dec number of windows in master
                       , ((modMask .|. shiftMask, xK_bracketleft), sendMessage $ IncMasterN 1)
                       , ((modMask .|. shiftMask, xK_bracketright), sendMessage $ IncMasterN (-1))
                       -- | rotate slave stack
                       , ((modMask, xK_comma), rotSlavesDown)
                       , ((modMask, xK_period), rotSlavesUp)
                       , ((modMask .|. shiftMask, xK_comma), rotAllDown)
                       , ((modMask .|. shiftMask, xK_period), rotAllUp)
                       -- | jump directly to master area
                       , ((modMask, xK_BackSpace), windows W.focusMaster)
                       , ((modMask .|. shiftMask, xK_BackSpace), windows $ W.focusMaster . (W.modify' promoteSlave'))
                       , ((modMask, xK_Tab), nextMatch History (return True))
                       -- | warp mouse to screen or banish
                       , ((modMask .|. shiftMask,   xK_z     ), warpToWindow (0) (0))
                       , ((modMask, xK_Escape), banish UpperLeft)
                       , ((modMask .|. shiftMask, xK_Escape), banishScreen UpperLeft)
                       -- | navigate all open applications
                       -- , ((modMask .|. shiftMask, xK_g), goToSelected def)
                       , ((modMask .|. shiftMask, xK_g), mygridselectWorkspace def (\ws -> W.greedyView ws . W.shift ws) )
                       , ((modMask .|. controlMask, xK_g), mygridselectWorkspace def (\ws -> W.greedyView ws) )
                       , ((modMask, xK_0), gotoMenuConfig def { menuCommand = "dmenu"
                                                                  , XMonad.Actions.WindowBringer.menuArgs = ["-p","Goto","-i","-l","10"]
                                                                  , windowTitler = decorateName
                                                                  })
                       , ((modMask .|. shiftMask, xK_0), bringMenuConfig def { menuCommand = "dmenu"
                                                                  , XMonad.Actions.WindowBringer.menuArgs = ["-p","Bring","-i","-l","10"]
                                                                  , windowTitler = decorateName
                                                                  })
                       , ((modMask, xK_p), spawn "dmenu_run -i") -- %! Launch dmenu
                       -- , ((mod4Mask, xK_k), spawn "kubenavmenu") -- %! Launch kubenavmenu
                       , ((modMask .|. controlMask, xK_s), spawn "sshmenu") -- %! Launch sshmenu
                       , ((modMask .|. controlMask, xK_p), spawn "passmenu") -- %! Launch passmenu
                       , ((modMask .|. shiftMask, xK_p), spawn "rofi-run")
                       , ((modMask, xK_equal), spawn "sudo backlight -inc 10")
                       , ((modMask, xK_minus), spawn "sudo backlight -dec 10")
                       , ((modMask .|. shiftMask, xK_equal), spawn "amixer -D pulse sset Master 10%+")
                       , ((modMask .|. shiftMask, xK_minus), spawn "amixer -D pulse sset Master 10%-")
                       -- recompiling (possibly generate help here)
                       , ((modMask, xK_q), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
                       , ((modMask .|. shiftMask, xK_slash), helpCommand) -- %! Run xmessage with a summary of the default keybindings (useful for beginners)
                     ]
                     -- | warp mouse to active window
                     ++ [((modMask .|. controlMask, key), warpToScreen sc (0) (0)) 
                                | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]]
                     -- | rename workspace (swap id-s only)
                     ++ [
                         ((modMask .|. controlMask, k), swapWithCurrent i) | (i, k) <- zip myWorkspaces [xK_1 ..]
                     ]
                     -- | goto workspace with moving focus to another monitor instead of bringing workspace to current monitor
                     ++ [((m .|. modMask, k), windows $ f i) 
                             | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
                             , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]  -- view replaces greedyView (swapping workspaces)
                     ) 
   `additionalKeysP` [
                       ("<XF86Explorer>", spawn "scrot -z `date +\"%Y-%m-%d_%H-%M-%S\"`-all.png && notify-send \"All screens saved\"")
                       , ("<XF86HomePage>", spawn "scrot -u -z `date +\"%Y-%m-%d_%H-%M-%S\"`-win.png && notify-send \"Active window saved\"")
                       -- , ("<XF86HomePage>", spawn "scrot -u -z `date +\"%Y-%m-%d_%H-%M-%S\"`-win.png")
                       -- testing only
                       -- , ("<XF86Tools>", winCount >>= \d->spawn $ "notify-send "++d )
                       , ("<XF86Tools>", spawn $ "notify-send \"$(/home/darek/.scripts/sound_check)\"" )
                       , ("S-<XF86Tools>", spawn $ "/home/darek/.scripts/sound_duplex && notify-send \"$(/home/darek/.scripts/sound_check)\"" )
                       -- focusuje ale wysyla spacje tylko 1 raz i jak nie trafi w guzik unmute to nie dziala
                       -- , ("<XF86Tools>", spawn $ "xdotool search \"Google Chrome\" windowactivate --sync key --clearmodifiers space" )
                       
                       , ("<XF86AudioLowerVolume>", spawn "amixer sset Master,0 10%-")
                       , ("<XF86AudioRaiseVolume>", spawn "amixer sset Master,0 10%+")
                       -- , ("<XF86AudioMute>", spawn "amixer sset Capture,0 toggle")
                       , ("<XF86AudioMute>", spawn "amixer sset Capture,0 nocap")
                       , ("S-<XF86AudioMute>", spawn "amixer sset Capture,0 cap")
                       , ("C-<XF86AudioMute>", spawn "amixer sset Master,0 toggle")
                       -- , ("<XF86Sleep>", spawn $ "notify-send "++"Sleep") -- testing: prevent sleep; not working -> computer goes to sleep anyway
                       -- | Move the focused window
                       , ("M-<U>", withFocused (keysMoveWindow (0, -15)))
                       , ("M-<D>", withFocused (keysMoveWindow (0, 15)))
                       , ("M-<L>", withFocused (keysMoveWindow (-15, 0)))
                       , ("M-<R>", withFocused (keysMoveWindow (15, 0)))
                       -- | Resize the focused window
                       , ("M-S-<U>", withFocused (keysResizeWindow (0, -15) (0, 0)))
                       , ("M-S-<D>", withFocused (keysResizeWindow (0, 15) (0, 0)))
                       , ("M-S-<R>", withFocused (keysResizeWindow (15, 0) (0, 0)))
                       , ("M-S-<L>", withFocused (keysResizeWindow (-15, 0) (0, 0)))
                       -- | Go to the next / previous workspace
                       -- , ("M-C-<R>", nextWS)
                       -- , ("M-C-<L>", prevWS)
                       , ("M-C-<R>", moveTo Next nonNSP)
                       , ("M-C-<L>", moveTo Prev nonNSP)
                       , ("M-C-<U>", swapNextScreen)
                       , ("M-C-<D>", swapPrevScreen)
                       , ("M-o", incWindowSpacing 3)
                       , ("M-i", setScreenWindowSpacing defaultSpacing)
                       , ("M-u", decWindowSpacing 3)
                       -- | confirm quitting       
                       , ("M-S-q", dchoice ["-p","Exit?"] ["Exit","Reboot","Shutdown"] [
                                                                                (io exitSuccess)
                                                                                , (spawn "sudo /sbin/reboot")
                                                                                , (spawn "sudo /sbin/poweroff")])
                     ]
   `additionalMouseBindings` [
                       -- | right click to maximize window
                       -- ((modMask, 3), (\w -> focus w >> mouseResizeWindow w >> ifClick (windows $ W.float w $ W.RationalRect 0 0 1 1)))
                       -- | navigate workspaces with mouse
                       ((modMask, 6), const $ prevWS)
                       , ((modMask, 7), const $ nextWS)
                       -- | swap screens with mouse
                       , ((modMask, 8), const $ swapNextScreen)
                     ]
     where
       modMask = mod1Mask -- myModMask
       nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
       nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))
       helpCommand :: X ()
       helpCommand = spawn ("echo " ++ show help ++ " | xmessage -file -")
       decorateName :: X.WindowSpace -> Window -> X String
       decorateName ws w = do
               name <- show <$> getName w
               current <- gets (W.currentTag . windowset)
               getWorkspaceName (W.tag ws) >>= return . format (current) (W.tag ws) (name) . fromMaybe "" 
               where format :: WorkspaceId -> WorkspaceId -> String -> String -> String
                     -- format cur tag wn wsn = (if cur == tag then "*" else " ") ++ "[" ++ tag ++ ":" ++ pad wsn ++ "] " ++ wn
                     format cur tag wn wsn = "[" ++ tag ++ ":" ++ pad wsn ++ "]" ++ (if cur == tag then "*" else " ") ++ " " ++ wn
                     pad :: String -> String
                     pad s = concat [s, replicate (max 0 (8 - length s)) ' '] 
               -- return $ (if current == W.tag ws then "*" else "") ++ "[" ++ W.tag ws ++ ":" wsname ++ "] " ++ name

myLayouts = ["tall","wide","ndock","pdock","full","stack","accordion","three","grid","auto","float"]
tall   = renamed [Replace "tall"]      $ minimize $ maximize $ spacing defaultSpacing $ ResizableTall 1 (3/100) (1/2) []
wide   = renamed [Replace "wide"]      $ Mirror $ tall
-- dock   = renamed [Replace "dock"]      $ minimize $ maximize $ spacing 3 $ TwoPane (3/100) (1/2) -- old dock with 1 window only available in master area
ndock  = renamed [Replace "ndock"]     $ minimize $ maximize $ spacing defaultSpacing $ NewDock 1 (3/100) (1/2)
pdock  = renamed [Replace "pdock"]      $ minimize $ maximize $ spacing defaultSpacing $ TwoPanePersistent Nothing (3/100) (1/2)
full   = renamed [Replace "full"]      $ minimize $ noBorders $ Full
stack  = renamed [Replace "stack"]     $ minimize $ maximize $ spacing defaultSpacing $ StackTile 1 (3/100) (1/2)
acc    = renamed [Replace "accordion"] $ minimize $ maximize $ spacing defaultSpacing $ Accordion
three  = renamed [Replace "three"]     $ minimize $ maximize $ spacing defaultSpacing $ ThreeColMid 1 (3/100) (1/2)
grid   = renamed [Replace "grid"]      $ minimize $ maximize $ spacing defaultSpacing $ Grid
autom  = renamed [Replace "auto"]      $ minimize $ maximize $ spacing defaultSpacing $ Mirror $ autoMaster 1 (1/100) Grid
flt    = renamed [Replace "float"]     $ minimize $ maximize $ simplestFloat

myWorkspaces = map show $ [1..9]
myScratchpads = [
  NS "scratchpad" "alacritty --class scratchpad --title scratchpad -e /usr/local/bin/tmux-run" 
     (stringProperty "WM_NAME" =? "scratchpad") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.5)
     ]

help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "",
    "-- Workspaces & screens",
    "mod-[1..9]         Switch to workSpace N",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging",
    "",
    "EOF"]
