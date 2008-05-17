module FrakData where

import Graphics.Rendering.Cairo.Matrix

import Control.Monad
import Data.List

type Set a = a -> Bool
type Point = (Double, Double)
type IFS = [Matrix]
	
type InvMatrix = Matrix

applyS :: Matrix -> Set Point -> Set Point
applyS m = applySinv (invert m)

applySinv :: InvMatrix -> Set Point -> Set Point
applySinv im s = \p -> s $ transformPoint im p

-- Shapes

square, fullSquare :: Set Point
square     = fromCorners (1/4) (3/4) (1/4) (3/4)
fullSquare = fromCorners 0 1 0 1
lineSet    = fromCorners 0 1 (4/10) (6/10)
triangle (x,y) = 0 <= y && y <= 1 && abs (2*(x-1/2)) <= y

fromCorners :: Double -> Double -> Double -> Double -> Set Point
fromCorners x1 x2 y1 y2 (x,y) = 
	x1 <= x && x <= x2 && y1 <= y && y <= y2

rot_square = applyS (rotateMiddle 1 identity) square

rotateAround (x,y) r = translate x y . rotate r . translate (-x) (-y)
scaleAround (x,y) s = translate x y . scale s s . translate (-x) (-y)

rotateMiddle = rotateAround (1/2, 1/2)
scaleMiddle  = scaleAround (1/2, 1/2)

sierpinsky =
	[ scaleMiddle (1/2) . translate 0 (-1/2) $ identity
	, scaleMiddle (1/2) . translate (1/2) (1/2) $ identity
	, scaleMiddle (1/2) . translate (-1/2) (1/2) $ identity
	]

koch =  [ scaleMiddle (1/3) . translate (-1) (0) $ identity
	, scaleMiddle (1/3) . rotateAround (0, 1/2) (-pi/3)  $ identity
	, scaleMiddle (1/3) . rotateAround (1, 1/2) ( pi/3)  $ identity
	, scaleMiddle (1/3) . translate ( 1) (0) $ identity
	]

conway = [ translate ( 1/4) 0 . scaleMiddle (sqrt 2 / 2) . rotateMiddle ( pi/4) $ identity
	 , translate (-1/4) 0 . scaleMiddle (sqrt 2 / 2) . rotateMiddle (-pi/4) $ identity
	 ]

runIFS :: Integer -> IFS -> Set Point ->  Set Point
runIFS n ifs base = run' n
 where run' 0 = base
       run' n = step $ run' (n-1)
       step s = \p -> or (map (\im -> (applySinv im s) p) inv)
       inv = map invert ifs

ifsStep :: IFS -> Set Point -> Set Point
ifsStep ms s = \p -> or (map (\m -> (m `applyS` s) p) ms)

ifsCode :: Int -> IFS ->  Point -> Maybe [Int]
ifsCode n ifs point = ifsCode' n point
  where ifsCode' 0 p = [] `justIf` (fullSquare p)
        ifsCode' n p = msum $ map try (zip [0..] inv)
	  where try (i,m) = (i:) `fmap` ifsCode' (n-1) (transformPoint m p)
	inv = map invert ifs

-- Some general utilities
doMB mb action = maybe (return ()) action mb

something `justIf` True = Just something
something `justIf` False = Nothing

readM :: (Monad m, Read t) => String -> m t
readM s = case readsPrec 0 s of
            [(v,"")] -> return v
            _        -> fail "readM: parse error"


-- Geometric stuff
-- Lets use Cairos stuff directly.

{- 
type Column = (Double, Double, Double)
type Matrix = (Column, Column, Column)


normalize (a,b,c) = (a/c, b/c)

apply m@((a,b,c),(d,e,f),(g,h,i)) p@(x,y) = normalize
	( a * x + b * y + c 
	, d * x + e * y + f
	, g * x + h * y + i
	)

inv :: Matrix -> Matrix
inv m@((a,b,c),(d,e,f),(g,h,i)) = scale (1 / det m)
	( (e*i - f*h, c*h - b*i, b*f - c*e) 
	, (f*g - d*i, a*i - c*i, c*d - a*f) 
	, (d*h - e*g, b*g - a*h, a*e - b*d) 
	)

scale :: Double -> Matrix -> Matrix
scale s m@((a,b,c),(d,e,f),(g,h,i)) = ((s*a,s*b,s*c),(s*d,s*e,s*f),(s*g,s*h,s*i))

det :: Matrix -> Double
det m@((a,b,c),(d,e,f),(g,h,i)) = a * e * i
                                + b * f * g
                                + c * d * h
				- c * e * g
				- a * f * h
				- b * d * i

identity :: Matrix
identity = ((1,0,0),(0,1,0),(0,0,1))

-}
