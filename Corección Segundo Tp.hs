-- APROBADO

import Data.Char (ord, chr)

--Aca lo único que hice fue modificar las funciones "ord" y "chr" cómo pedían para
--que funcionen con el tipo de dato "Integer"

chr1 :: Integer->Char
chr1 n = chr (fromInteger n)

ord1 :: Char->Integer
ord1 c = fromIntegral (ord c)

--Funciones de clases anteriores que voy a necesitar para estos ejercicios

--Clase 9

--Dados "a" y "b" , utiliza el algoritmo de Euclides extendido para obtener una
--tripla ((a : b), s,t) tal que sa + tb = (a : b)
emcd :: Integer->Integer->(Integer,Integer,Integer)
emcd a b | a>b = (max,s,t)
         | otherwise = (max, t,s)
 where (max,s,t) = emcd1 a b

emcd1 :: Integer->Integer->(Integer,Integer,Integer)
emcd1 a b | b==0 && a<0 = (abs a, (-1),2)
          | b==0 && a>0 = (abs a, 1,2)
          | a<b = emcd1 b a
          | otherwise = (maxi, t, s - t*(div a b))
          where
          (maxi, s, t) = emcd1 b (mod a b)

--Clase 10


--Resuelve una ecuacion lineal de congruencia
ecEquivalente :: (Integer, Integer, Integer) -> (Integer,Integer,Integer)
ecEquivalente (a ,b , m) | mod b d == 0 = (div a d, div b d, div m d)
                         | otherwise = undefined
   where (d,s,t) = emcd a m

solucionEcCoprima :: (Integer, Integer, Integer) -> (Integer, Integer)
solucionEcCoprima (a, b,m) = (mod (b*s) m , m)
     where (d,s,t) = emcd a m

solucionEc :: (Integer, Integer, Integer)->(Integer,Integer)
solucionEc e = solucionEcCoprima (ecEquivalente e)



--- Funciones del Tp ---

--EJERCICIO 1
generarClaves :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer))
generarClaves p q | (p*q)<=127= undefined
                  | otherwise = ( (p*q , d), (p*q , e) )
                 where m = (p-1)*(q-1)
                       (d, r) = solucionEc (e,1, m)
                       e = eligeE m (m-2)

-- CORRECCIÓN: conviene usar _ cuando no necesitás usar un valor: (d, _) = ...

--Elige el número "e" tal que 2<=e<= m-2 y además sea coprimo con m.
eligeE :: Integer->Integer->Integer
eligeE m 2 = undefined                 --Esto sucedería si no puede encontrar ningún "e" que cumpla lo requerido.
eligeE m candidato | sonCoprimos m candidato = candidato
                   | otherwise = eligeE m (candidato -1)

-- CORRECCIÓN: sería más claro un nombre que te diga lo que hace, por ejemplo "mayorCoprimoHasta".

sonCoprimos :: Integer-> Integer-> Bool
sonCoprimos n1 n2 = max == 1
              where (max,nolou,so) = emcd n1 n2

-- CORRECCIÓN: conviene usar _ cuando no necesitás usar un valor: (max, _, _) = ...,
-- mejor aún sería usar mcd en lugar de emcd, no solo porque así evitás hacer cálculos innecesarios
-- si no también por declaratividad.

--Cuándo pongo "mensaje" en las funciones me refiero a un String, osea un conjunto de caracteres.
--EJERCICIO 2
encriptar :: (Integer, Integer) -> String -> [Integer]
encriptar clave men = encriptaListaEnteros clave lista
               where lista = mensajeAListaEnteros men

mensajeAListaEnteros :: String -> [Integer]
mensajeAListaEnteros [] = []
mensajeAListaEnteros s =  (ord1 (head s)) : (mensajeAListaEnteros (tail s))

encriptaListaEnteros :: (Integer,Integer)->[Integer]-> [Integer]
encriptaListaEnteros _ [] = []
encriptaListaEnteros (n,d) (x:xs) = (mod (x^d) n) : (encriptaListaEnteros (n,d) xs)

-- OK

--EJERCICIO 3
desencriptar :: (Integer, Integer) -> [Integer] -> String
desencriptar clave lista = listaEnterosAMensaje listaDesen
                      where listaDesen = desencriptaListaEnteros clave lista

desencriptaListaEnteros :: (Integer,Integer)->[Integer]-> [Integer]
desencriptaListaEnteros _ [] = []
desencriptaListaEnteros (n,e) (x:xs) = (mod (x^e) n) : (desencriptaListaEnteros (n,e) xs)

listaEnterosAMensaje :: [Integer] -> String
listaEnterosAMensaje [] = []
listaEnterosAMensaje (x:xs) =  (chr1 x) : (listaEnterosAMensaje xs)

-- OK

--Opcional
--Decidi buscar la factorización en primos de "100337" ya que no me pareció un número muy grande,
--aunque seguramente esta forma no funcione para otros número acá es suficiente.

esDivisibleDesde :: Integer->Integer->Bool
esDivisibleDesde n m   | (n `mod` m) == 0 = False
                       | ceiling(sqrt(fromIntegral n)) < m  = True
                       | otherwise = esDivisibleDesde n (m+1)

esPrimo :: Integer-> Bool
esPrimo n =  n>1 && (esDivisibleDesde n 2)

factorizacionPrimosDesde :: Integer->Integer->(Integer,Integer)
factorizacionPrimosDesde n divi | n <= divi = (1,n)
                                | (mod n divi) == 0 && ( (esPrimo divi) && (esPrimo  (div n divi)) ) = (divi, div n divi)
                                | otherwise = factorizacionPrimosDesde n (divi+1)

factorizacionPrimos :: Integer->(Integer,Integer)
factorizacionPrimos n = factorizacionPrimosDesde n 1

--Con esto se que 100337 = 269*373. Por lo tanto m = 268*372= 99696, y resolviendo el sistema
-- 60953*e = 1 (99696) me da que "e"=1001. Por lo tanto la clave privada es (100337,1001).
--Mi mensaje:
cpriv = (100337, 1001)
mje = [78387,38913,23881,97660,22839,77756,85626,23881,77756,85626,23881,220,23881,96986,74457,28700,18800,78982,1606,58255,28700,77756,23881,99961,77756,50740,23881,96986,220,58255,22329,23881,77756,220,22839,99961,22839,77756,50740,23881,77756,96657,38913,23881,85626,58255,77756,85626,23881,74457,18800,28700,77756,96657,38913,23881,77756,50740,18800,77756,78982,18800,91658,91658,58255,77756,96593,58255,438,22839,28700,18800,1606,58255,77756,99961,58255,77756,85626,23881,220,74457,38913,11695,28700,18800,77756,23881,96986,77756,99961,58255,77756,74457,38913,58255,28700,23881,96986,1606,23881,96986,58255,77756,61099,77756,23881,220,77756,38913,96986,58255,77756,78982,18800,91658,91658,58255,77756,74457,58255,220,23881,28700,58255,77756,96657,38913,23881,77756,43745,18800,74457,18800,50740,22839,220,77756,74457,22839,96986,77756,43745,22839,96986,97660,22839,220,13699,77756,78982,18800,50740,23881,96986,1606,22839,96986,13699,77756,50740,22839,91658,91658,58255,28700,23881,99961,99961,58255,77756,61099,77756,22839,1606,28700,58255,220,77756,23881,220,78982,23881,74457,18800,58255,220,41020,77756,38246,220,77756,38913,96986,58255,77756,85626,23881,99961,18800,74457,18800,58255,41020,77756,97672,22791,24493,77756,10069,38913,61099,77756,11695,38913,23881,96986,58255,220,77756,99961,58255,220,77756,74457,99961,58255,220,23881,220]
