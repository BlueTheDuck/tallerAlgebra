import Clase3;

eAprox :: Integer -> Float
eAprox 0 = 1
eAprox n = eAprox (n - 1) + fromIntegral 1 / fromIntegral(Clase3.fac n)

e = eAprox 10

sumaPotencias :: Float -> Integer -> Integer -> Float
sumaPotencias q 1 1 = q*q
sumaPotencias q n 1 = q**(a+1) + sumaPotencias q (n-1) 1
                    where a = fromIntegral n
sumaPotencias q 1 m = q**(b+1) + sumaPotencias q 1 (m-1)
                    where b = fromIntegral m
sumaPotencias q n m = q**(a+b) + sumaPotencias q n (m-1) + sumaPotencias q (n-1) m - sumaPotencias q (n-1) (m-1)
                    where a = fromIntegral n
                          b = fromIntegral m