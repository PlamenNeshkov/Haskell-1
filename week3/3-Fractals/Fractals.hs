module Fractals where

import Data.Complex (Complex(..), magnitude)


-- The complex number a + bi corresponds to the point (a, b)
type Point = Complex Double
type Coordinate = (Double, Double)
-- A Fractal is a function which given a coordinate
-- returns the number of iterations before diverging
type Fractal = Int -> Int -> Int

-- Number of iterations after which we say that a point doesn't diverge
maxIterations :: Num a => a
maxIterations = 127

-- The size of the image we'll colour
screen :: Num a => (a, a)
screen = (600, 600)

-- The part of the fractal we want to see
window :: (Coordinate, Coordinate)
window = ((-1.9, 1.3), (0.7, -1.3)) 

-- Mapping a pixel to a coordinate
pixelToCoordinate :: Int -> Int -> (Coordinate, Coordinate) -> Coordinate
pixelToCoordinate x y ((minx, maxy), (maxx, miny)) = (minx + rangex * (fromIntegral x / fst screen), 
                                                      miny + rangey * (fromIntegral y / snd screen)) 
                                                     where rangex = maxx - minx
                                                           rangey = maxy - miny
                                                            

-- Implement the formula for Mandelbrot's fractal
mandelbrot :: Point -- Coordinate for calculation
           -> Point -- Current z value
           -> Int   -- Number of current iteration
           -> Int   -- Iterations before diverging

mandelbrot c z iter | magnitude z > 2 = iter
                    | iter >= maxIterations = iter
                    | otherwise = mandelbrot c (z^2 + c) (iter+1)

-- Returning the number of iterations needed for given
-- point to diverge
drawMandelbrot :: Int -> Int -> Int
drawMandelbrot x y = mandelbrot (x' :+ y') (0 :+ 0) 0
  where (x', y') = pixelToCoordinate x y window
