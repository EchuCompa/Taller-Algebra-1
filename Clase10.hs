module Clase10
where
import Clase9
import Clase_adicional
import Clase5

--Ejercicio 1
--Resuelve una ecuacion lineal de congruencia
ecEquivalente :: (Integer, Integer, Integer) -> (Integer,Integer,Integer)
ecEquivalente (a ,b , m) | mod b d == 0 = (div a d, div b d, div m d)
                         | otherwise = undefined
   where d = mcd a m

solucionEcCoprima :: (Integer, Integer, Integer) -> (Integer, Integer)
solucionEcCoprima (a, b,m) = (mod (b*s) m , m)
     where (d,s,t) = emcd a m  

solucionEc :: (Integer, Integer, Integer)->(Integer,Integer)
solucionEc e = solucionEcCoprima (ecEquivalente e)

--Ejercicio 2
--Dado un sistema lineal de ecuaciones de congruencia, devuelve un sistema simplificado 
--equivalente
sisteSimpliEqui :: [(Integer, Integer,Integer)]->[(Integer,Integer)]
sisteSimpliEqui [] = []
sisteSimpliEqui (x:xs) = (solucionEc x) : (sisteSimpliEqui xs)

--Ejercicio 3
--Devuelve una lista con todos los primos malos para un sistema simplificado.
--Primo malo = Divide a + de 2 módulos del sistema

modulos :: [(Integer, Integer)] -> [Integer]
modulos [] = []
modulos ((r, m):es) = m:(modulos es)

mayorModulo :: [(Integer, Integer)] -> Integer
mayorModulo sist = maximum (modulos sist)

cotaParaPrimoMaloDesde :: [(Integer, Integer)] -> Integer -> Integer
cotaParaPrimoMaloDesde sist n | nEsimoPrimo (n+1) > (mayorModulo sist) = n
                              | otherwise = cotaParaPrimoMaloDesde sist (n+1)

cotaParaPrimoMalo :: [(Integer, Integer)] -> Integer
cotaParaPrimoMalo sist = cotaParaPrimoMaloDesde sist 1

cantidadMultiplos :: [Integer] -> Integer -> Integer
cantidadMultiplos [] _ = 0
cantidadMultiplos (m:ms) n | mod m (nEsimoPrimo n) == 0 = 1 + cantidadMultiplos ms n
                           | otherwise = cantidadMultiplos ms n 

esPrimoMalo :: [(Integer, Integer)] -> Integer -> Bool
esPrimoMalo sist n = cantidadMultiplos (modulos sist) n >= 2


todosLosPrimosMalosHasta :: [(Integer, Integer)] -> Integer -> [Integer]
todosLosPrimosMalosHasta _ 0 = []
todosLosPrimosMalosHasta sist n | esPrimoMalo sist n = (nEsimoPrimo n):(todosLosPrimosMalosHasta sist (n-1))
                                | otherwise = todosLosPrimosMalosHasta sist (n-1)

todosLosPrimosMalos :: [(Integer, Integer)] -> [Integer]
todosLosPrimosMalos [] = []
todosLosPrimosMalos sist = todosLosPrimosMalosHasta sist (cotaParaPrimoMalo sist)

--Ejercicio 4
--Resuelve un sistema en el que todos los modulos son potencias de un mismo primo.
solucDosEcPotenciasPrimoOrd :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
solucDosEcPotenciasPrimoOrd (r1, m1) (r2, m2) | mod (r2-r1) m1 == 0 = (r2, m2)
                                              | otherwise = undefined

solucDosEcPotenciasPrimo :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
solucDosEcPotenciasPrimo (r1, m1) (r2, m2) | m1 <= m2 = solucDosEcPotenciasPrimoOrd (r1, m1) (r2, m2)
                                           | otherwise = solucDosEcPotenciasPrimoOrd (r2, m2) (r1, m1)

solucSistemaPotenciasPrimo :: [(Integer, Integer)] -> (Integer, Integer)
solucSistemaPotenciasPrimo [e] = e
solucSistemaPotenciasPrimo (e1:e2:es) = solucSistemaPotenciasPrimo ((solucDosEcPotenciasPrimo e1 e2):es)

--Ejercicio 5
--Dado un sistema y un primo p, devuelve los dos sistemas que surgen al desdoblar 
--cada ecuacion del sistema original segun el primo p como se explico anteriormente.

desdoblarSistemaEnFcionPrimo :: [(Integer, Integer)] -> Integer -> ([(Integer, Integer)], [(Integer, Integer)])
desdoblarSistemaEnFcionPrimo [] _ = ([], [])
desdoblarSistemaEnFcionPrimo ((r, m):es) p | k == 0 = (pri, (r, m):seg)
                                           | m == p^k = ((r, m):pri, seg)
                                           | otherwise = ((mod r (p^k), p^k):pri, (mod r (div m (p^k)), div m (p^k)):seg)
 where (pri, seg) = desdoblarSistemaEnFcionPrimo es p 
       k = qEsimaPotencia m p

sistemaEquivSinPrimosMalosAux :: [(Integer, Integer)] -> [Integer] -> [(Integer, Integer)]
sistemaEquivSinPrimosMalosAux sist [] = sist
sistemaEquivSinPrimosMalosAux sist (p:ps) = (solucSistemaPotenciasPrimo pri):(sistemaEquivSinPrimosMalosAux seg ps)  
 where (pri, seg) = desdoblarSistemaEnFcionPrimo sist p

sistemaEquivSinPrimosMalos :: [(Integer, Integer)] -> [(Integer, Integer)]
sistemaEquivSinPrimosMalos sist = sistemaEquivSinPrimosMalosAux sist (todosLosPrimosMalos sist)

--Ejercicio 6
--Resuelve un sistema simplificado en el que los m´odulos son coprimos dos a dos
solucSistemaModCoprimos :: [(Integer, Integer)] -> (Integer, Integer)
solucSistemaModCoprimos [e] = e
solucSistemaModCoprimos (e1:e2:e) = solucSistemaModCoprimos ((mod (r2*m1*s + r1*m2*t) (m1*m2), m1*m2):e)
                     where (r1, m1) = e1
                           (r2, m2) = e2
                           (d , s, t) = emcd m1 m2

solucSistema :: [(Integer, Integer, Integer)] -> (Integer, Integer)
solucSistema sist = solucSistemaModCoprimos ( sistemaEquivSinPrimosMalos ( sisteSimpliEqui sist) )

---TAREA
--Ejercicio 1
--Dado un sistema general, decide si cada una de sus ecuaciones vista independientemente de las otras,
--tiene solucion
cadaEcTieneSoluc :: [(Integer, Integer, Integer)] -> Bool 
cadaEcTieneSoluc [] = True
cadaEcTieneSoluc ((a,b,m):e) = (mod b (mcd a m)) == 0 && (cadaEcTieneSoluc e)

--Ejercicio 2
--Dado un sistema simplificado, decide si tiene solucion
tieneSolucionSimplif :: [(Integer, Integer)] -> Bool
tieneSolucionSimplif [] = True
tieneSolucionSimplif ((r,m):e) = (revisaEcuEquivalentes (r,m) e) && tieneSolucionSimplif e

--Revisa si todas las ecuaciones de un sistema son "compatibles" con la ecuación (r,m). 
revisaEcuEquivalentes :: (Integer, Integer) -> [(Integer,Integer)]->Bool
revisaEcuEquivalentes _ [] = True
revisaEcuEquivalentes (r,m) ((r1,m1):e) | mod m m1 == 0 = (mod (r-r1) m1 == 0) && (revisaEcuEquivalentes (r,m) e)
                                        | mod m1 m == 0 = (mod (r1-r) m == 0) &&  (revisaEcuEquivalentes (r,m) e)
                                        | otherwise = (revisaEcuEquivalentes (r,m) e)
--Ejercicio 3
--Dado un sistema decide si tien solución
tieneSolucion :: [(Integer, Integer, Integer)]->Bool
tieneSolucion e = (cadaEcTieneSoluc e) && (tieneSolucionSimplif (sisteSimpliEqui e))

--Ejercicio 4
--Dados dos n´¡umeros coprimos r y m con 1 ≤ r < m, encuentra un numero primo en la clase 
--de congruencia X ≡ r (mod m)
dirichletTrucho :: Integer->Integer->Integer
dirichletTrucho r m = buscaPrimo (r,m) 0

--Busca un primo con ciertas características desde un número "k"
buscaPrimo :: (Integer,Integer)->Integer->Integer
buscaPrimo (r,m) k | esPrimo (r+m*k) = r +m*k
                   | otherwise = buscaPrimo (r,m) (k+1)

