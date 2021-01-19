-- Devolución: El TP está en general bien. Hay algunas funciones con casos que no andan
-- y otras que se pueden escribir mejor.


--Funciones de clases pasadas que voy a utilizar:

--Suma todos los divisores de un número m, desde cierto divisor m
sumaDivisoresDesde :: Integer->Integer->Integer
sumaDivisoresDesde n m | n == m = n
                       | (n `mod` m) == 0 =  m + sumaDivisoresDesde n (m+1)
                       | otherwise = sumaDivisoresDesde n (m+1)

esPrimo :: Integer-> Bool
esPrimo n =  n>1 && (sumaDivisoresDesde n 2) == n

--De ahora en más son los ejercicios del tp

--Es una función que analiza si comparten algún divisor además del 1, comenzando desde un número div.
sonCoprimosAux :: Integer -> Integer -> Integer -> Bool
sonCoprimosAux n1 n2 div |  n1<div || n2<div = True
                         | ((n1 `mod` div) == 0) && ((n2 `mod` div) == 0) = False
                         | otherwise = sonCoprimosAux n1 n2 (div+1)

--CORRECCIÓN: es mejor poner un nombre más declarativo en lugar de usar "aux",
-- por ejemplo "noExisteDivisorComunDesde".

sonCoprimos :: Integer-> Integer-> Bool
sonCoprimos n1 n2 | (esPrimo n1 && esPrimo n2) = True
                  | otherwise = sonCoprimosAux n1 n2 2

--CORRECCIÓN: el primer caso no es correcto, por ejemplo si n1==n2
-- y ambos son primos daría True, pero no son coprimos.

es2PseudoPrimo :: Integer->Bool
es2PseudoPrimo n = ((2^(n-1) - 1) `mod` n == 0) && not (esPrimo n)

--CORRECCIÓN: si n==1 devuelve True pero el 1 no es 2-pseudoPrimo.

--Define si un número n es un a-pseudoprimo
esAPseudoPrimo :: Integer->Integer->Bool
esAPseudoPrimo n a = (a^(n-1) - 1) `mod` n == 0 && not (esPrimo n)

--CORRECCIÓN: misma corrección que "es2Pseudoprimo".

cantidad3PseudoPrimos :: Integer->Integer
cantidad3PseudoPrimos n | n<=90 = 0
                        | esAPseudoPrimo n 3  = cantidad3PseudoPrimos (n-1) + 1
                        | otherwise = cantidad3PseudoPrimos (n-1)

--CORRECCIÓN: no se entiende qué significa el primer caso o de dónde sale el 90.
-- La mejora en tiempo de ejecución es despreciable y el código queda confuso.

kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo k | k==1 = 1105
                       | otherwise = proximo2y3Pseudo (1 + kesimo2y3Pseudoprimo (k-1) )

--No le pongo un "freno" puesto que asumo que en algún momento encontrará un número que cumpla esta condición
--Busca el primer número mayor o igual que n que sea 2 y 3 pseudoprimo
proximo2y3Pseudo :: Integer->Integer
proximo2y3Pseudo n | esAPseudoPrimo n 3 && esAPseudoPrimo n 2 = n
                   | otherwise = proximo2y3Pseudo (n+1)

-- CORRECCIÓN: está bien no ponerle freno pero no hay que asumir nada.
-- Si no hubiera un próximo 2y3-pseudoprimo la función debe indefinirse.


esCarmichael :: Integer->Bool
esCarmichael n = esCarmichaelAux n 2

--Analiza si todos los a entre 1 y n-1 que sean coprimos con n también son aPseudoPrimos con n.
esCarmichaelAux :: Integer->Integer->Bool
esCarmichaelAux n a | a==n = True
                    | sonCoprimos a n && not (esAPseudoPrimo n a) = False
                    | otherwise = esCarmichaelAux n (a+1)

-- CORRECCIÓN: se cuelga si n=1.

--Funciones que no tienen relevancia pero use para comprobar si lo había hecho bien
proximoCarmi :: Integer->Integer
proximoCarmi n | esCarmichael n = n
               | otherwise = proximoCarmi (n+1)

nEsimoCarmi :: Integer->Integer
nEsimoCarmi n | n==1 = 561
              | otherwise = proximoCarmi (1 + nEsimoCarmi (n-1))
