-- Funciones auxiliares
esPar :: Integer -> Bool
esPar n = (n `mod` 2) == 0

-- |Verifica si `a` divide a `b`
divide :: Integer -> Integer -> Bool
divide a b = (b `mod` a) == 0

-- |Cuenta la cantidad de divisores de `a` que estan en el rango [1, `b`]
contarDivisoresDesde :: Integer -> Integer -> Integer
contarDivisoresDesde a 1 = 1
contarDivisoresDesde a b | b `divide` a = 1 + contarDivisoresDesde a (b - 1)
                         | otherwise = contarDivisoresDesde a (b - 1)

-- |Shorthand para `contarDivisoresDesde a a`
contarDivisores :: Integer -> Integer
contarDivisores a = 1 + contarDivisoresDesde a (a - 1)

-- |Verifica de forma recursiva si un número es primo
esPrimo :: Integer -> Bool
esPrimo 0 = False
esPrimo 1 = False
esPrimo n = (contarDivisores n) == 2

-- |Verifica que los valores de la tupla sean primos
sonPrimos (a, b) = esPrimo a && esPrimo b

-- Ej 1
satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach 0 = False
satisfaceGoldbach 1 = False
satisfaceGoldbach 2 = False
satisfaceGoldbach n | n_es_par && n_es_suma_primos = True
                    | otherwise = False
                    where n_es_par = esPar n
                          n_es_suma_primos = not (descomposicionEnPrimos n == (0, 0))



-- Ej 2
{-|
      Verifica la conjetura de Goldbach para todos los numeros pares en (2, `n`].
      Gracias al shortcircuiting, si `satisfaceGoldbach n` devuelve `False`, esta funcion
      no continua con la recursividad, por lo que solo hay que definir un caso base
-}
verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta 4 = True
verificarConjeturaHasta n = (satisfaceGoldbach n) && (verificarConjeturaHasta (n-2))



-- Ej 3

-- |Toma un `n` par mayor a 2 e intenta encontrar dos numeros primos que al sumarlos den `n`
descomposicionEnPrimos :: Integer -> (Integer, Integer)
descomposicionEnPrimos n = descompPartiendoDesde n 2

{-|
      Busca un `(a, b)` primos que al sumarlos den `n`. Prueba cada combinación 
      partiendo desde `(n - t, t)` hasta `(0, n)`. `n` tiene que ser par y mayor a 2; 
      `t` tiene que ser menor a `n`. Devuelve `(0, 0)` si no encuentra ninguno
-}
descompPartiendoDesde n t | sonPrimos (n - t, t) = (n - t, t)
                          | n - t <= 2 = (0, 0)
                          | otherwise = descompPartiendoDesde n (t + 1)


-- Ej 4
-- |Toma un `n` par mayor a dos, y calcula la cantidad de tuplas que permiten verificar la conjetura
numeroDeDescomposiciones :: Integer -> Integer
numeroDeDescomposiciones n = numeroDeDescompDesde n 2

{-|
      Calcula de forma recursiva la cantidad de tuplas ordenadas, con numeros 
      primos mayores a 2 que al sumarlos dan `n`. Saltea las tuplas cuyo 
      segundo número es menor a `t`
-}
numeroDeDescompDesde n t | skip == 0 = 0 
                         | otherwise = 1 + numeroDeDescompDesde n (skip+1)
                         where skip = snd (descompPartiendoDesde n t)