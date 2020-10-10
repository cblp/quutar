module Square (square, solveSquareEquation) where

square :: Double -> Double
square x = x * x

-- a x² + b x + c = 0
solveSquareEquation a b c =
  -- D = b² - 4 a c
  -- x₁,₂ = (- b ± √D) / 2 a
  let d = b * b - 4 * a * c
  in
    if a == 0 then
      undefined
    else if d < 0 then
      []
    else
      -- [ (- b - sqrt d) / (2 * a)
      -- , (- b + sqrt d) / (2 * a)
      -- ]
      [ ((- b) ± sqrt d) / (2 * a)
      | (±) <- [(-), (+)]
      ]

main :: IO ()
main =
  print (solveSquareEquation 1 (-2) 1)
