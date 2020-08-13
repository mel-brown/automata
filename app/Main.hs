module Main where

import Square
import GridState
import SDL
import SDL.Event
import SDL.Vect
import Data.Vector.Storable (fromList)
import Control.Monad (guard, unless)
import Control.Monad.RWS (runRWS)
import Foreign.C.Types (CInt)

main :: IO ()
main = do
    initializeAll
    window   <- createWindow "Automaton" (automatonWindow 600 600)
    renderer <- createRenderer window (-1) defaultRenderer
    (_, s, w) <- appLoop renderer ((), seedGlider 300 300, []) conway
    mapM_ putStrLn w
    -- print s

automatonWindow :: CInt -> CInt -> WindowConfig
automatonWindow w h = WindowConfig
    { windowBorder          = True
    , windowHighDPI         = False
    , windowInputGrabbed    = False
    , windowMode            = Windowed
    , windowGraphicsContext = NoGraphicsContext
    , windowPosition        = Wherever
    , windowResizable       = True
    , windowInitialSize     = V2 w h
    , windowVisible         = True
    }

appLoop :: Renderer -> (a, Grid, [String]) -> Rule -> IO ((), Grid, [String])
appLoop renderer (a, s, w) rule = do
    drawGridToScreen s renderer
    events <- pollEvents
    if | keyPressed KeycodeP events -> pauseLoop renderer (a, s, w) rule
       | keyPressed KeycodeQ events -> pure ((), s, w)
       | otherwise ->
           let (a', s', w') = runAutomaton tick rule s
           in  appLoop renderer (a', s', w ++ w') rule

pauseLoop :: Renderer -> (a, Grid, [String]) -> Rule -> IO ((), Grid, [String])
pauseLoop renderer (a, s, w) rule = do
    drawGridToScreen s renderer
    events <- pollEvents
    if | windowSizeChanged events -> do
            let V2 w' h' = head [ windowSizeChangedEventSize | Event _ (WindowSizeChangedEvent (WindowSizeChangedEventData{..})) <- events ]
            pauseLoop renderer ((), resizeGrid (fromIntegral w') (fromIntegral h') s, w ++ ["window resized"]) rule
       | any isLeftClick events -> do
            let toggleList = map ((\(V2 x y) -> Square (x `div` 3, y `div` 3)) . leftClickPosition) $ filter isLeftClick events
                s' = toggleSquares toggleList (const True) s
            pauseLoop renderer ((), s', w ++ ["toggled pixels on"]) rule
       | any isRightClick events -> do
            let toggleList = map ((\(V2 x y) -> Square (x `div` 3, y `div` 3)) . rightClickPosition) $ filter isRightClick events
                s' = toggleSquares toggleList (const False) s
            pauseLoop renderer ((), s', w ++ ["toggled pixels off"]) rule
       | keyPressed KeycodeP events -> appLoop renderer (a, s, w) rule
       | otherwise -> pauseLoop renderer (a, s, w) rule

-- One cell is a 3x3 square
drawGridToScreen :: Grid -> Renderer -> IO ()
drawGridToScreen s renderer = do
    rendererDrawColor renderer $= V4 255 255 255 255
    clear renderer
    rendererDrawColor renderer $= V4 0 0 0 255
    fillRects renderer . fromList . map makeRectangle $ aliveList s
    present renderer

makeRectangle :: Square -> Rectangle CInt
makeRectangle (Square (r, c)) = Rectangle (P $ V2 (3 * fromIntegral r) (3 * fromIntegral c)) (V2 3 3)

eventIsKeyPress :: Keycode -> Event -> Bool
eventIsKeyPress keycode = (\case
    KeyboardEvent KeyboardEventData{..} -> keyboardEventKeyMotion == Pressed && keysymKeycode (keyboardEventKeysym) == keycode
    _ -> False
    ) . eventPayload

keyPressed :: Keycode -> [Event] -> Bool
keyPressed = any . eventIsKeyPress

isWindowSizeChange :: Event -> Bool
isWindowSizeChange = (\case
    WindowSizeChangedEvent _ -> True
    _ -> False) . eventPayload

windowSizeChanged :: [Event] -> Bool
windowSizeChanged = any isWindowSizeChange

isLeftClick :: Event -> Bool
isLeftClick = (\case
    MouseButtonEvent (MouseButtonEventData{..}) -> mouseButtonEventMotion == Pressed && mouseButtonEventButton == ButtonLeft && mouseButtonEventClicks == 1
    _ -> False
    ) . eventPayload

isRightClick :: Event -> Bool
isRightClick = (\case
    MouseButtonEvent (MouseButtonEventData{..}) -> mouseButtonEventMotion == Pressed && mouseButtonEventButton == ButtonRight && mouseButtonEventClicks == 1
    _ -> False
    ) . eventPayload

leftClickPosition :: Event -> V2 Int
leftClickPosition e = case eventPayload e of
    MouseButtonEvent (MouseButtonEventData{..}) ->
        if   mouseButtonEventButton == ButtonLeft
        then let P v2 = mouseButtonEventPos in fromIntegral <$> v2
        else error "not a left click..!"
    _ -> error "not a left click..!"

rightClickPosition :: Event -> V2 Int
rightClickPosition e = case eventPayload e of
    MouseButtonEvent (MouseButtonEventData{..}) ->
        if   mouseButtonEventButton == ButtonRight
        then let P v2 = mouseButtonEventPos in fromIntegral <$> v2
        else error "not a right click..!"
    _ -> error "not a right click..!"