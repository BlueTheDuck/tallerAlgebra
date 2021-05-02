module Clase3 (fac)
where
    fib :: Integer -> Integer
    fib 0 = 0
    fib 1 = 1
    fib n = fib(n-1) + fib(n-2)

    fac :: Integer -> Integer
    fac 0 = 1
    fac 1 = 1
    fac n = n * fac (n-1)

    divPor3 :: Integer -> Bool
    divPor3 0 = True
    divPor3 1 = False
    divPor3 2 = False
    divPor3 n = divPor3 (n - 3)

    sumaImpares :: Integer -> Integer
    sumaImpares 1 = 1
    sumaImpares n = (2*n - 1) + sumaImpares (n-1)

    sumarDigs :: Integer -> Integer
    sumarDigs n | n < 10 = n
                | otherwise = (n `mod` 10) + sumarDigs (n `div` 10)


    digitosIguales :: Integer -> Bool
    digitosIguales a | a < 10 = True
                     | otherwise = (a `mod` 10) == ( b `mod` 10) && digitosIguales(b)
                     where b = a `div` 10