absoluto x | x >= 0 = x
           | x < 0 = -x

maximo :: Ord a => a -> a -> a
maximo a b | a > b = a
           | otherwise = b

maxAbs a b = maximo (absoluto a) (absoluto b)

maximo3 a b c = maximo (a) (maximo b c)

algunoEs0 a b | a == 0 = True
              | b == 0 = True
              | otherwise = False

algunoEs0requiem a 0 = True
algunoEs0requiem 0 b = True
algunoEs0requiem a b = False

ambosSon0 :: Float -> Float -> Bool
ambosSon0 a b = a == 0 && a == b

ambosSon0requiem :: Float -> Float -> Bool
ambosSon0requiem 0 0 = True
ambosSon0requiem a b = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b = (a `mod` b) == 0

digitoUnidades :: Int -> Int
digitoUnidades n = n `mod` 10

digitoDecenas :: Int -> Int
digitoDecenas n = ((n `mod` 100) - (n `mod` 10)) `div` 10

