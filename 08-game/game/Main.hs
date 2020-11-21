{-# LANGUAGE NamedFieldPuns #-}

import           Graphics.Gloss (Display (InWindow), Picture, black,
                                 circleSolid, color, play, rectangleSolid,
                                 rotate, text, thickCircle, translate, white)
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
  { birdY  :: Float
  , birdY' :: Float
  , gaps   :: [Gap]
  , score  :: Int
  }

data Gap = Gap
  { x, bottom, top :: Float
  }

initialWorld :: World
initialWorld =
  World
    { birdY = 0
    , birdY' = 0
    , gaps =
        [ Gap{x = 100, bottom = -100, top = 100}
        , Gap{x = 500, bottom =    0, top = 200}
        ]
    , score = 0
    }

render :: World -> Picture
render World{birdY, birdY', gaps, score} =
  color white $ bird <> foldMap renderGap gaps <> renderScore score
  where
    eye  = translate 20 20 (circleSolid 10)
    skin = thickCircle birdRadius 10
    beak = translate birdRadius 0 (rectangleSolid birdRadius 10)
    bird = translate 0 birdY $ rotate pitch $ skin <> eye <> beak
    birdRadius = 50
    pitch = - atan2 birdY' 300 / pi * 180

renderGap :: Gap -> Picture
renderGap Gap{x, bottom, top} =
  mconcat
    [ translate x (- h / 2 + bottom) $ rectangleSolid gapWidth h
    , translate x (  h / 2 + top   ) $ rectangleSolid gapWidth h
    ]
  where
    gapWidth = 20

renderScore :: Int -> Picture
renderScore score = translate 0 (h / 4) $ text $ show score

onEvent :: Event -> World -> World
onEvent event world@World{birdY'} =
  case event of
    EventKey (SpecialKey KeySpace) Down _ _ -> world{birdY' = birdY' + 500}
    _                                       -> world

onTick :: Float -> World -> World
onTick dt world@World{birdY, birdY', gaps}
  | birdY > gameFloor, birdY < gameCeiling =
      world
        { birdY = birdY + birdY' * dt
        , birdY' = birdY' + gravity * dt
        , gaps = [gap{x = x - dt * xSpeed} | gap@Gap{x} <- gaps]
        }
  | otherwise =
      initialWorld
  where
    gameFloor = - h / 2
    gameCeiling = h / 2
    gravity = -200
    xSpeed = 100
