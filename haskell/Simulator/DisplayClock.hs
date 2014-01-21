module Simulator.DisplayClock (TimeVar,
                               displayClock,
                               CommandThread,
                               ClockCommands(..),
                               makeCommandThread) where

import Control.Monad.Reader
import Control.Concurrent
import Data.Word
import Data.IORef
import qualified Graphics.UI.SDL as SDL

type TimeVar = MVar (Int, Int, Int)
type CommandThread = TimeVar -> MVar () -> IO ()

displayClock :: CommandThread -> IO ()
displayClock commandThread = do
  syncLock <- newMVar ()
  timeVar <- newMVar (0,0,0)
  forkIO (commandThread timeVar syncLock)
  displayThread timeVar syncLock

displayThread :: TimeVar -> MVar () -> IO ()
displayThread timeVar syncLock = SDL.withInit [SDL.InitVideo] $ do
  screen <- SDL.setVideoMode 960 400 32 [SDL.HWSurface, SDL.DoubleBuf]
  SDL.setCaption "Processor demo: Clock" ""
  let color2px = SDL.mapRGB (SDL.surfaceGetPixelFormat screen)
  white      <- color2px 0 0   0
  lightGreen <- color2px 0 255 0
  darkGreen  <- color2px 0 0  0
  continueLoop <- newIORef True
  accel <- newIORef False
  initialTick <- SDL.getTicks
  runReaderT (eventLoop 0) $ ELD {
    _graphicsData = GD { _screen = screen
                       , _white = white
                       , _lightGreen = lightGreen
                       , _darkGreen = darkGreen
                       }
    , _continueLoop = continueLoop
    , _accel = accel
    , _timeVar = timeVar
    , _syncLock = syncLock
    , _initialTick = initialTick
    }
  return ()

data EventLoopData = ELD { _graphicsData :: GraphicsData
                         , _continueLoop :: IORef Bool
                         , _accel        :: IORef Bool
                         , _timeVar      :: TimeVar
                         , _syncLock     :: MVar ()
                         , _initialTick  :: Word32
                         }
data GraphicsData = GD { _screen       :: SDL.Surface
                       , _white        :: SDL.Pixel
                       , _lightGreen   :: SDL.Pixel
                       , _darkGreen    :: SDL.Pixel
                       }

type EventLoopMonad a = ReaderT EventLoopData IO a

eventLoop :: Word32 -> EventLoopMonad ()
eventLoop secondsPreviouslyElapsed = do
  continueLoop <- lift . readIORef =<< asks _continueLoop
  when continueLoop $ do
    syncLock <- asks _syncLock
    
    handleEvents

    accel <- lift . readIORef =<< asks _accel

    newTick <- lift SDL.getTicks
    initialTick <- asks _initialTick
    let secondsElapsed = (newTick - initialTick) `div` 1000
    when (secondsElapsed > secondsPreviouslyElapsed || accel) $ do
      lift $ tryPutMVar syncLock ()
      return ()
      
    displayTime
    
    lift $ SDL.delay 10
    eventLoop secondsElapsed

displayTime :: EventLoopMonad ()
displayTime = do
  time <- lift . readMVar =<< asks _timeVar
  graphicsData <- asks _graphicsData
  let screen = _screen graphicsData
  lift $ do
    SDL.fillRect screen Nothing (_white graphicsData)
    draw7segs graphicsData time
    SDL.flip screen

handleEvents :: EventLoopMonad ()
handleEvents = do
  e <- lift SDL.pollEvent
  case e of
    SDL.NoEvent -> return ()
    _ -> handleEvent e >> handleEvents
  where handleEvent SDL.Quit = quit
        handleEvent (SDL.KeyUp (SDL.Keysym {SDL.symKey = k})) = case k of
          SDL.SDLK_ESCAPE -> quit
          SDL.SDLK_RETURN -> lift . flip writeIORef False =<< asks _accel
          _ -> return ()
        handleEvent (SDL.KeyDown (SDL.Keysym {SDL.symKey = k})) = case k of
          SDL.SDLK_RETURN -> lift . flip writeIORef True =<< asks _accel
          _ -> return ()
        handleEvent _ = return ()
        quit = lift . flip writeIORef False =<< asks _continueLoop


digitTo7Seg :: Int -> [Bool]
digitTo7Seg 0 = [       True,
                  True,       True,
                        False,
                  True,       True,
                        True
                ]
digitTo7Seg 1 = [       False,
                  False,      True,
                        False,
                  False,      True,
                        False
                ]
digitTo7Seg 2 = [       True,
                  False,      True,
                        True,
                  True,       False,
                        True
                ]
digitTo7Seg 3 = [       True,
                  False,      True,
                        True,
                  False,      True,
                        True
                ]
digitTo7Seg 4 = [       False,
                  True,       True,
                        True,
                  False,      True,
                        False
                ]
digitTo7Seg 5 = [       True,
                  True,       False,
                        True,
                  False,      True,
                        True
                ]
digitTo7Seg 6 = [       True,
                  True,       False,
                        True,
                  True,       True,
                        True
                ]
digitTo7Seg 7 = [       True,
                  False,      True,
                        False,
                  False,      True,
                        False
                ]
digitTo7Seg 8 = [       True,
                  True,       True,
                        True,
                  True,       True,
                        True
                ]
digitTo7Seg 9 = [       True,
                  True,       True,
                        True,
                  False,      True,
                        True
                ]
                
draw7segs :: GraphicsData -> (Int,Int,Int) -> IO ()
draw7segs (GD { _screen = screen
              , _lightGreen = lightGreen
              , _darkGreen = darkGreen
              })
          (hr,mn,sec) = do
  let decompose n = [n `div` 10, n `mod` 10]
      digits = concatMap decompose [hr, mn, sec]
  zipWithM_ (\d x -> drawDigit d x origY)
            digits
            [ origX + k * width
                    + ((k+1) `div` 2) * bigMargin
                    + (k `div` 2) * hugeMargin
            | k <- [0..5] ]
  where drawDigit d offsX offsY = do
          let segments =
                [ horizontal (offsX + th + mg) (offsY)
                , vertical offsX (offsY + th + mg)                    
                , vertical (offsX + th + 2*mg + lg) (offsY + th + mg)         
                , horizontal (offsX + th + mg) (offsY + th + 2*mg + lg)       
                , vertical offsX (offsY + 2*th + 3*mg + lg)                   
                , vertical (offsX + th + 2*mg + lg) (offsY + 2*th + 3*mg + lg)
                , horizontal (offsX + th + mg) (offsY + 2*th + 4*mg + 2*lg)
                ]
              light = map snd . filter fst         . zip (digitTo7Seg d)
                      $ segments
              dark  = map snd . filter (not . fst) . zip (digitTo7Seg d)
                      $ segments
          -- in order to handle overlapping segments (negative margins)
          -- draw the dark segments first
          forM_ dark  $ \seg -> seg darkGreen
          forM_ light $ \seg -> seg lightGreen

        horizontal = drawBar lg th
        vertical   = drawBar th lg
        drawBar w h x y color = SDL.fillRect screen (Just $ SDL.Rect x y w h) color
        th = 16 -- segment thickness
        lg = 64 -- segment length
        mg = -3 -- hahaha negative margins!
        width = 2*th + 2*mg + lg
        bigMargin = 30
        hugeMargin = 70

        totalWidth = 6*(2*th + 2*mg + lg) + 3*bigMargin + 2*hugeMargin
        totalHeight = 3*th + 4*mg + 2*lg

        origX = (960 - totalWidth ) `div` 2
        origY = (400 - totalHeight) `div` 2
        

 
-- Clock advancement controlled by another thread

data ClockCommands = ClockCommands { waitNextSec :: IO ()
                                   , setHour     :: Int -> IO ()
                                   , setMinute   :: Int -> IO ()
                                   , setSecond   :: Int -> IO ()
                                   }

makeCommandThread :: (ClockCommands -> IO ()) -> CommandThread
makeCommandThread f timeVar syncLock = 
  f $ ClockCommands {
    setHour   = \h -> modifyMVar_ timeVar (\(_,m,s) -> return (h,m,s)), 
    setMinute = \m -> modifyMVar_ timeVar (\(h,_,s) -> return (h,m,s)),
    setSecond = \s -> modifyMVar_ timeVar (\(h,m,_) -> return (h,m,s)),
    waitNextSec = takeMVar syncLock
    }
  

