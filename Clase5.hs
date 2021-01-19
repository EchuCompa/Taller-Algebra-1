module Clase5
where
--Suma todos los divisores de un número m, desde cierto divisor m
sumaDivisoresDesde :: Integer->Integer->Integer
sumaDivisoresDesde n m | n == m = n
                       | (n `mod` m) == 0 =  m + sumaDivisoresDesde n (m+1)
                       | otherwise = sumaDivisoresDesde n (m+1)

--Nada por aquí, nada por allá 
--NO FUNCA CON 
esPrimoAux n m | m > sqrt (n) = True
               | (mod n m) == 0 = False
               | otherwise = esPrimoAux n (m+1)


esPrimo :: Integer-> Bool
esPrimo n =  n>1 && (sumaDivisoresDesde n 2) == n

--Encuntra el menor número primo desde un número n
minimoPrimoDesde :: Integer-> Integer
minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoPrimoDesde (n+1)
--Devuelve el valor del n-esimo primo que se ingrese
nEsimoPrimo :: Integer-> Integer
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n-1))

---Factorial
fact :: Integer->Integer
fact 0 = 1
fact n = n * fact (n-1)

--Ejercicio 6
--Devuelve el menor factorial desde cierto número m, comenzando por un número n
menorFactDesdeDesde :: Integer->Integer->Integer
menorFactDesdeDesde n m | m<= fact n = n
                        | otherwise = menorFactDesdeDesde (n+1) m

--Devuelve el menor factorial desde cierto número m
menorFactDesde :: Integer->Integer
menorFactDesde m = fact (menorFactDesdeDesde 1 m)

--Ejercicio 7
----Devuelve el mayor factorial hsata cierto número m
mayorFactHasta :: Integer->Integer
mayorFactHasta m =  fact (menorFactDesdeDesde 1 m -1)

----Ejercicio 8
--Se cacha la wea
esFact :: Integer->Bool
esFact n = (menorFactDesde n) == n

--Fibonacci
fibo :: Integer->Integer
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

--Emcuentra el menor número de Fibonacci acierto número m, contando desde cierto Fibo.
menorFiboDesde :: Integer->Integer->Integer
menorFiboDesde n m | (fibo n) >= m = fibo n
                   | otherwise = menorFiboDesde (n+1) m

--Ejercicio 9
--Determina si un número es parte de la sucesión de Fibonacci
esFibo :: Integer->Bool
esFibo n = (menorFiboDesde 1 n) == n

--Determina si n es igual a la suma de los primeros m números primos
esSumaInicialDePrimos' :: Integer->Integer->Bool
esSumaInicialDePrimos' n m | n<=0 = n==0
                          | otherwise = esSumaInicialDePrimos' (n - (nEsimoPrimo m)) (m+1)

--Ejercicio 10
esSumaInicialDePrimos :: Integer->Bool
esSumaInicialDePrimos n = esSumaInicialDePrimos' n 1

--Devuelve la suma de todos los divisores de un número n
sumaDivisores :: Integer->Integer
sumaDivisores n = sumaDivisoresDesde n 1

--Devuelve un número con la mayor suma de sus divisores entre dos números n1 y n2
tomarValorMax' :: Integer->Integer->Integer->Integer
tomarValorMax' n1 n2 t | t > n2 = n1
                       | sumaDivisores n1 >= sumaDivisores t = tomarValorMax' n1 n2 (t+1)
                       | otherwise = tomarValorMax' (t) n2 (t+1)

--Ejercicio 11
tomarValorMax :: Integer->Integer->Integer
tomarValorMax n1 n2 = tomarValorMax' n1 n2 (n1+1)

--Ejercicio 12
--Devuelve un número con la menor suma de sus divisores entre dos números n1 y n2
tomarValorMin :: Integer->Integer->Integer
tomarValorMin n1 n2 = tomarValorMin' n1 n2 (n1+1)

--Devuelve un número con la menor suma de sus divisores entre dos números n1 y n2
tomarValorMin' :: Integer->Integer->Integer->Integer
tomarValorMin' n1 n2 t | t > n2 = n1
                       | sumaDivisores n1 <= sumaDivisores t = tomarValorMin' n1 n2 (t+1)
                       | otherwise = tomarValorMin' (t) n2 (t+1)

--Ejercicio 13
--Determina si un número n es suma de dos primos
esSumaDeDosPrimos :: Integer->Bool
esSumaDeDosPrimos n = esSumaDeDosPrimos' n 1


--Determina si un número n es suma de dos primos, mayores a al m-esimo número primo
esSumaDeDosPrimos' :: Integer->Integer->Bool
esSumaDeDosPrimos' n m | m >=n = False
                       | otherwise = esPrimo (n - nEsimoPrimo m) || esSumaDeDosPrimos' n (m+1)

--Ejercicio 14
--Determina si se cumple la conjetura de Goldbach hasta cierto número n
goldbach :: Integer->Bool
goldbach n = n<=4 || esSumaDeDosPrimos n && goldbach (n-2)

--Ejercicio 15
--Devuelve cuántos primos gemelos hay menores a un número n. 
primosGem :: Integer->Integer
primosGem n = primosGem' 3 n


--Devuelve cuántos primos gemelos hay desde un número n1, hasta n2.
primosGem' :: Integer->Integer->Integer
primosGem' n1 n2 | n1+1==n2 = 0
                 | esPrimo n1 && esPrimo (n1+2) =  1 + primosGem' (n1+1) n2
                 | otherwise = primosGem' (n1+1) n2

--Ejercicio 16
--Devuelve el primer par de primos gemelos ambos más grandes que n
proxPrimosGem :: Integer->(Integer,Integer)
proxPrimosGem n | esPrimo (n+1) && esPrimo (n+3) = (n+1,n+3)
                | otherwise = proxPrimosGem (n+1)

--Ejercicio 17
--Devuelve el largo de la secuencia de Collatz de un número n
largoSecuencia :: Integer->Integer
largoSecuencia n | n==1 = 0
                 | n `mod` 2 == 0 = largoSecuencia (n `div` 2 ) + 1
                 | otherwise = largoSecuencia (3*n+1) + 1

--Devuelve el número con la secuencia de Collatz más larga hasta el 10000
mayorCollatz :: Integer->Integer->Integer
mayorCollatz n m | m == 10000 = n
                 | largoSecuencia n >= largoSecuencia m = mayorCollatz n (m+1)
                 | otherwise = mayorCollatz m (m+1)
