import Control.Monad.State
import Graphics.UI.SDL as SDL

main :: IO ()
main = SDL.withInit [InitVideo] $ do
         screen <- SDL.setVideoMode 960 400 32 [HWSurface, DoubleBuf]
         SDL.setCaption "Processor demo: Clock" ""
         initial_tick <- SDL.getTicks
         white <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0   0   0
         green <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0   255 0
         execStateT (eventLoop screen initial_tick white green) True
         return ()

eventLoop screen initial_tick white green = do
  continueLoop <- get
  when continueLoop $ do
    handleEvents
    new_tick <- lift SDL.getTicks
    let time = (new_tick - initial_tick) `div` 1000
    lift $ do
      SDL.fillRect screen Nothing white
      draw7seg screen green 20 20
      draw7seg screen green (20 + width + bigMargin) 20
      draw7seg screen green (20 + 2*(width + bigMargin)) 20
      draw7seg screen green (20 + 3*(width + bigMargin)) 20
      draw7seg screen green (20 + 4*(width + bigMargin)) 20
      draw7seg screen green (20 + 5*(width + bigMargin)) 20
      
      -- SDL.blitSurface text Nothing screen (Just $ Rect 100 100 0 0)
      SDL.flip screen
      SDL.delay 50
    eventLoop screen initial_tick white green

handleEvents = do
  e <- lift SDL.pollEvent
  case e of
    NoEvent -> return ()
    _ -> handleEvent e >> handleEvents


th = 16 -- segment thickness
lg = 64 -- segment length
mg = 8 -- margin
width = 2*th + 2*mg + lg
bigMargin = 40

draw7seg screen green offsX offsY = do
  horizontal (offsX + th + mg) (offsY)
  vertical offsX (offsY + th + mg)
  vertical (offsX + th + 2*mg + lg) (offsY + th + mg)
  horizontal (offsX + th + mg) (offsY + th + 2*mg + lg)
  vertical offsX (offsY + 2*th + 3*mg + lg)
  vertical (offsX + th + 2*mg + lg) (offsY + 2*th + 3*mg + lg)
  horizontal (offsX + th + mg) (offsY + 2*th + 4*mg + 2*lg)
  where horizontal = drawBar lg th
        vertical   = drawBar th lg
        drawBar w h x y = SDL.fillRect screen (Just $ Rect x y w h) green

  

handleEvent Quit = put False
handleEvent (KeyUp (Keysym {symKey = SDLK_ESCAPE})) = put False
handleEvent _ = return ()
