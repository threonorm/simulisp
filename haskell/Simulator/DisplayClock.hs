import Control.Monad.Reader
import Control.Concurrent
import Data.Word
import Data.IORef
import qualified Graphics.UI.SDL as SDL

type TimeVar = MVar (Int, Int, Int)

main :: IO ()
main = do
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
  darkGreen  <- color2px 0 64  0
  continueLoop <- newIORef True
  initialTick <- SDL.getTicks
  runReaderT (eventLoop 0) $ ELD {
    _graphicsData = GD { _screen = screen
                       , _white = white
                       , _lightGreen = lightGreen
                       , _darkGreen = darkGreen
                       }
    , _continueLoop = continueLoop
    , _timeVar = timeVar
    , _syncLock = syncLock
    , _initialTick = initialTick
    }
  return ()

data EventLoopData = ELD { _graphicsData :: GraphicsData
                         , _continueLoop :: IORef Bool
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
    lift $ tryTakeMVar syncLock
    
    handleEvents
    
    newTick <- lift SDL.getTicks
    initialTick <- asks _initialTick
    let secondsElapsed = (newTick - initialTick) `div` 1000
    when (secondsElapsed > secondsPreviouslyElapsed) $ do
      lift $ tryPutMVar syncLock ()
      return ()
      
    displayTime
    
    lift $ SDL.delay 50
    eventLoop secondsElapsed

displayTime :: EventLoopMonad ()
displayTime = do
  time <- lift . readMVar =<< asks _timeVar
  graphicsData <- asks _graphicsData
  let screen = _screen graphicsData
  lift $ do
    SDL.fillRect screen Nothing (_white graphicsData)
    draw7segs graphicsData (12, 42, 07)
    SDL.flip screen

handleEvents :: EventLoopMonad ()
handleEvents = do
  e <- lift SDL.pollEvent
  case e of
    SDL.NoEvent -> return ()
    _ -> handleEvent e >> handleEvents
  where handleEvent SDL.Quit = quit
        handleEvent (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_ESCAPE})) = quit
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
                        True,
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
  zipWithM_ (\d x -> drawDigit d x 20)
            digits
            [ 20 + k * (width + bigMargin) | k <- [0..5] ]
  where drawDigit d offsX offsY = do
          zipWithM_
            (\switchOn seg -> if switchOn
                              then seg lightGreen
                              else seg darkGreen)
            (digitTo7Seg d)
            [ horizontal (offsX + th + mg) (offsY)
            , vertical offsX (offsY + th + mg)                    
            , vertical (offsX + th + 2*mg + lg) (offsY + th + mg)         
            , horizontal (offsX + th + mg) (offsY + th + 2*mg + lg)       
            , vertical offsX (offsY + 2*th + 3*mg + lg)                   
            , vertical (offsX + th + 2*mg + lg) (offsY + 2*th + 3*mg + lg)
            , horizontal (offsX + th + mg) (offsY + 2*th + 4*mg + 2*lg)
            ]
        horizontal = drawBar lg th
        vertical   = drawBar th lg
        drawBar w h x y color = SDL.fillRect screen (Just $ SDL.Rect x y w h) color
        th = 16 -- segment thickness
        lg = 64 -- segment length
        mg = 0  -- margin
        width = 2*th + 2*mg + lg
        bigMargin = 40

 
-- Clock advancement controlled by another thread

commandThread :: TimeVar -> MVar () -> IO ()
commandThread timeVar syncLock = undefined


