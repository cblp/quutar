{-# LANGUAGE NamedFieldPuns #-}

import           Graphics.Gloss                     (Display (InWindow),
                                                     Picture, black,
                                                     circleSolid, color, greyN,
                                                     play, rectangleSolid,
                                                     thickCircle, translate,
                                                     white)
import           Graphics.Gloss.Interface.Pure.Game (Event (EventKey),
                                                     Key (SpecialKey),
                                                     KeyState (Down),
                                                     SpecialKey (KeySpace))

main :: IO ()
main =
  play
    (InWindow "happy bird" (windowWidth, windowHeight) (0, 0))
    black
    fps
    initialWorld
    render
    onEvent
    onTick
  where
    fps = 30

windowWidth :: Int
windowWidth = 1200

windowHeight :: Int
windowHeight = 800

h :: Float
h = fromIntegral windowHeight

data World = World
  { birdY       :: Float
  , birdY'      :: Float
  , gaps        :: [Gap]
  , gamesFailed :: Integer
  , gapsPassed  :: Integer
  }

data Gap = Gap
  { x, bottom, top :: Float
  }

initialWorld :: World
initialWorld =
  World
    { birdY = 0
    , birdY' = 0
    , gaps = [Gap{x = 100, bottom = -100, top = 100}]
    , gamesFailed = 0
    , gapsPassed = 0
    }

render :: World -> Picture
render World{birdY, gaps} =
  bird <> foldMap renderGap gaps
  where
    body = color (greyN 0.5) (circleSolid birdRadius)
    eye  = translate 20 20 (circleSolid 10)
    skin = thickCircle birdRadius 10
    beak = translate birdRadius 0 (rectangleSolid birdRadius 10)
    bird = translate 0 birdY $ body <> color white (skin <> eye <> beak)
    birdRadius = 50

renderGap :: Gap -> Picture
renderGap Gap{x, bottom, top} =
  color white $
    mconcat
      [ translate x (- h / 2 + bottom) (rectangleSolid gapWidth h)
      , translate x (  h / 2 + top   ) (rectangleSolid gapWidth h)
      ]
  where
    gapWidth = 20

onEvent :: Event -> World -> World
onEvent event world@World{birdY'} =
  case event of
    EventKey (SpecialKey KeySpace) Down _ _ -> world{birdY' = birdY' + 500}
    _                                       -> world

onTick :: Float -> World -> World
onTick dt world@World{birdY, birdY', gaps, gamesFailed}
  | birdY > - h / 2, birdY < h / 2 =
      world
        { birdY = birdY + birdY' * dt
        , birdY' = birdY' + g * dt
        , gaps = map (\gap@Gap{x} -> gap{x = x - dt * 100}) gaps
        }
  | otherwise =
      initialWorld{gamesFailed = gamesFailed + 1}
  where
    g = -200
