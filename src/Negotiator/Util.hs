module Negotiator.Util where

takePosDivBy :: Int -> [a] -> [a]
takePosDivBy = takePosDivides' 0
    where
    takePosDivides' _ _ [] = []
    takePosDivides' pos n (x : xs)
        | pos `mod` n == 0 = x : next
        | otherwise = next
        where next = takePosDivides' (pos + 1) n xs

-- vector of 5 elements
newtype Vector5 = Vector5 
    { unwrap :: (Double,Double,Double,Double,Double) }
    deriving (Show, Eq)

v5fromList :: [Double] -> Vector5
v5fromList (v1 : v2 : v3 : v4 : v5 : rest) = Vector5 (v1,v2,v3,v4,v5)
v5fromList _ = undefined

v5dotp :: Vector5 -> Vector5 -> Double
v5dotp x y = x1 * y1 + x2 * y2 + x3 * y3 + x4 * y4 + x5 * y5
    where
    (x1,x2,x3,x4,x5) = unwrap x
    (y1,y2,y3,y4,y5) = unwrap y

