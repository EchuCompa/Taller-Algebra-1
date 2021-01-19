module Clase9
where
--Ejercicio 1
digitos :: Integer->Integer->[Integer]
digitos n b  | n<b = [n]
             | otherwise =(mod n b) : (digitos (div n b) b)

--Ejercicio 2 
numero :: [Integer] -> Integer -> Integer
numero lis b = numeroAux lis b 0

--Otra forma
numero2 :: [Integer] -> Integer -> Integer
numero2 [x] _ = x
numero2 (x:xs) b = x + b*(numero2 xs b)

--Función auxiliar que va contando las potencias a las que debe ser elevado b
numeroAux :: [Integer] -> Integer -> Integer->Integer
numeroAux [] _ _ = 0
numeroAux (x:xs) b p = x*(b^p) + ( numeroAux xs b (p+1) )

--Ejercicio 5
mcdDef :: Integer->Integer->Integer
mcdDef a 0 = abs a
mcdDef 0 b = abs b
mcdDef a b = maximo (interseccion (divisores a) (divisores b))

--Funciones que necesito de conjuntos
pertenece :: Integer->[Integer]->Bool
pertenece _ [] = False
pertenece x (y:ys) = x==y || pertenece x ys

interseccion :: [Integer]->[Integer]->[Integer]
interseccion [] _ = []
interseccion (x:xs) c2 | pertenece x c2 = x : (interseccion xs c2)
                       | otherwise = interseccion xs c2

divisores :: Integer->[Integer]
divisores n = divisoresAux n 1

--Agarra los divisores de un número desde cierto d
divisoresAux :: Integer->Integer->[Integer]
divisoresAux n d | d>n = []
              | mod n d == 0 = (d) : (divisoresAux n (d+1))
              | otherwise = divisoresAux n (d+1)

--Agarra el número de mayor valor de una lista
maximo :: [Integer]->Integer
maximo [x] = x
maximo (x:xs) | x > head xs = maximo (x : (tail xs))
              | otherwise = maximo xs

--Ejercicio 6
mcd :: Integer->Integer->Integer
mcd a 0 = abs a
mcd a b | a<b = mcd b a
        | otherwise = mcd b (mod a b)

--Ejercicio 8
mcm :: Integer->Integer->Integer
mcm _ 0 = 0
mcm 0 _ = 0
mcm a b = div (a*b) (mcd a b)

--Ejercicio 9
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

--Ejercicio 10
minSPosible :: Integer->Integer->(Integer,Integer)
minSPosible a b | max /= 1 = minSPosible  (div a max) (div b max)
                | otherwise = (s2,t2)
                 where 
                 (max,s1,t1) = emcd a b 
                 (s2, k) = (valorMasPeque s1 b 0) 
                 t2 = t1+a*(-k)


--Devuelve el valor positivo más pequeño posible de cierto n +- "q*k", y el valor que debe tomar "k".
valorMasPeque :: Integer->Integer->Integer->(Integer, Integer)
valorMasPeque n q k | n>=0 && n<q = (n,k)
                    | n<0 = valorMasPeque (n+q) q (k+1)
                    | n>0 = valorMasPeque (n-q) q (k-1)