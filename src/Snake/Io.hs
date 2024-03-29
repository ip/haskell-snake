{-# LANGUAGE OverloadedStrings #-}

module Snake.Io (
    initIo,
    getInputs,
    SnakeIo (..)
) where

import Control.Applicative
import GHC.Word (Word8)
import Snake.Core
import SDL
import SDL.Vect (V2 (..))
import SDL.Video.Renderer
import qualified Data.Text as Text

-- Exported

data SnakeIo = SnakeIo {
    drawFrame :: GameState -> IO ()
}

initIo :: IO SnakeIo
initIo = do
    initializeAll
    window <- createWindow "Haskell Snake" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer

    putStrLn ""
    logRendererInfo renderer

    let ioState = IoState {
        window_ = window,
        renderer_ = renderer,
        windowSize_ = fromIntegral <$> windowInitialSize defaultWindow
    } in return SnakeIo {
        drawFrame = drawFrame_ ioState
    }

logRendererInfo :: Renderer -> IO ()
logRendererInfo renderer = do
    rendererInfo <- getRendererInfo renderer
    let rendererName = Text.unpack $ rendererInfoName rendererInfo in
        putStrLn $ "Using renderer: " ++ rendererName

getInputs :: IO Inputs
getInputs = Inputs . (>>= keyboardEventToInputs) . maybeLast .
    filter isPressed . filter isKeyboardEvent . map eventPayload <$> pollEvents


-- Internal

data IoState = IoState {
    window_ :: Window,
    renderer_ :: Renderer,
    windowSize_ :: V2 Int
}

drawFrame_ :: IoState -> GameState -> IO ()
drawFrame_ ioState state = do
    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer

    drawFood ioState state
    drawSnake ioState state

    present renderer
        where renderer = renderer_ ioState

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just $ last xs

isKeyboardEvent :: EventPayload -> Bool
isKeyboardEvent (KeyboardEvent _) = True
isKeyboardEvent _                 = False

isPressed :: EventPayload -> Bool
isPressed (KeyboardEvent keyboardEvent) =
    keyboardEventKeyMotion keyboardEvent == Pressed

keyboardEventToInputs :: EventPayload -> Maybe Direction
keyboardEventToInputs (KeyboardEvent keyboardEvent) =
    scanCodeToDirection $ keysymScancode (keyboardEventKeysym keyboardEvent)

scanCodeToDirection :: Scancode -> Maybe Direction
scanCodeToDirection ScancodeRight = Just DirectionRight
scanCodeToDirection ScancodeDown  = Just DirectionDown
scanCodeToDirection ScancodeLeft  = Just DirectionLeft
scanCodeToDirection ScancodeUp    = Just DirectionUp
scanCodeToDirection _             = Nothing


drawFood :: IoState -> GameState -> IO ()
drawFood ioState state = drawTile ioState color pos
        where pos = foodPosition state
              color = V4 255 255 0 255

drawSnake :: IoState -> GameState -> IO ()
drawSnake ioState state = mapM_ drawLink snakeBody_
        where snakeBody_ = snakeBody state
              drawLink = drawTile ioState color
              color = V4 0 0 255 255

drawTile :: IoState -> V4 Word8 -> V2 Int -> IO ()
drawTile ioState color pos = do
    rendererDrawColor renderer $= color
    fillRect renderer $ Just $ fromIntegral <$> rect
        where renderer = renderer_ ioState
              rect = rectToWindowSpace windowSize2 worldSpaceRect
              worldSpaceRect = Rectangle (P pos) $ V2 1 1
              windowSize2 = windowSize_ ioState

rectToWindowSpace :: V2 Int -> Rectangle Int -> Rectangle Int
rectToWindowSpace winSize rect =
    let Rectangle (P a) b = rect in
        Rectangle (P $ vecToWindowSpace_ a) $ vecToWindowSpace_ b
            where vecToWindowSpace_ = vecToWindowSpace winSize

vecToWindowSpace :: V2 Int -> V2 Int -> V2 Int
vecToWindowSpace winSize vec = (vec * winSize) `divV2` fieldSize

divV2 :: V2 Int -> V2 Int -> V2 Int
divV2 = liftA2 div
