import Control.Monad.State
import Graphics.UI.SDL as SDL

main :: IO ()
main = SDL.withInit [InitVideo] $ do
         screen <- SDL.setVideoMode 960 400 32 [HWSurface, DoubleBuf]
         SDL.setCaption "Processor demo: Clock" ""
         initial_tick <- SDL.getTicks
         let color2px = SDL.mapRGB (SDL.surfaceGetPixelFormat screen)
         white      <- color2px 0 0   0
         lightGreen <- color2px 0 255 0
         darkGreen  <- color2px 0 64  0
         execStateT (eventLoop screen initial_tick white lightGreen darkGreen) True
         return ()

eventLoop screen initial_tick white lightGreen darkGreen = do
  continueLoop <- get
  when continueLoop $ do
    handleEvents
    new_tick <- lift SDL.getTicks
    let time = (new_tick - initial_tick) `div` 1000
    lift $ do
      SDL.fillRect screen Nothing white
      draw7segs screen lightGreen darkGreen (12, 42, 07)
      SDL.flip screen
      SDL.delay 50
    eventLoop screen initial_tick white lightGreen darkGreen

handleEvents = do
  e <- lift SDL.pollEvent
  case e of
    NoEvent -> return ()
    _ -> handleEvent e >> handleEvents

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
                

draw7segs screen lightGreen darkGreen (h,m,s) = do
  let decompose n = [n `div` 10, n `mod` 10]
      digits = concatMap decompose [h, m, s]
  zipWithM (\d x -> drawDigit d x 20)
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
        drawBar w h x y color = SDL.fillRect screen (Just $ Rect x y w h) color
        th = 16 -- segment thickness
        lg = 64 -- segment length
        mg = 0  -- margin
        width = 2*th + 2*mg + lg
        bigMargin = 40

  

handleEvent Quit = put False
handleEvent (KeyUp (Keysym {symKey = SDLK_ESCAPE})) = put False
handleEvent _ = return ()
