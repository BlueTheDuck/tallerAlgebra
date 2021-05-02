neg n = -n 

fib :: Int -> Int
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib(n-1) + fib(n-2)

fact :: Int -> Int
fact 0 = 1
fact 1 = 1
fact n | n > 0 = n*fact(n-1)
       | n < 0 = 0

min :: Ord a => a -> a -> a
min m n | m < n = m
        | m > n = n
        | otherwise = m

max m n | m > n = m
        | m < n = n
        | otherwise = m

-- Llamar como `sumatoria 1 10 (\ i n -> n)`
sumatoria :: Int -> Int -> (Int -> Int -> Int) -> Int
sumatoria i n f | i < n = (f i n) + sumatoria (i+1) n f
                | i > n = (f i n) + sumatoria (i-1) n f
                | i == n = (f i n)


