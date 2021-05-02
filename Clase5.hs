module Clase5 ()
where
    menorDivisorDesde :: Int -> Int -> Int
    menorDivisorDesde 0 _ = 1
    menorDivisorDesde 1 _ = 1
    menorDivisorDesde n d | n `mod` d == 0 = d
                          | otherwise = menorDivisorDesde n (d+1)
    menorDivisor :: Int -> Int
    menorDivisor 0 = 1
    menorDivisor 1 = 1
    menorDivisor n = menorDivisorDesde n 2

    esPrimo :: Int -> Bool
    esPrimo n = (menorDivisor n) == n

    calcPrimoRec :: Int -> Int -> Int
    calcPrimoRec n i | esPrimo n == True && i > 0 = calcPrimoRec (n+1) (i-1)
                     | esPrimo n == True && i == 0 = n
                     | esPrimo n == False = calcPrimoRec (n+1) i
    nEsimoPrimo :: Int -> Int
    nEsimoPrimo i = calcPrimoRec 1 i

    largaSecuencia :: Int -> Int
    largaSecuencia n = largaSecuenciaP n 0

    largaSecuenciaP :: Int -> Int -> Int
    largaSecuenciaP n p | n < 2 = p
                        | n `mod` 2 == 0 = largaSecuenciaP (n `div` 2) (p + 1)
                        | n `mod` 2 == 1 = largaSecuenciaP (3 * n + 1) (p + 1)

    lotharCollatz :: Int -> Int
    lotharCollatz 0 = 0
    lotharCollatz n | n == 1 = 1
                    | n `mod` 2 == 0 = lotharCollatz (n `div` 2)
                    | n `mod` 2 == 1 = lotharCollatz (3*n+1)