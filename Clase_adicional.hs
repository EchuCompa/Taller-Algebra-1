module Clase_adicional
where
import Clase5

---Ejercicios de la clase
--Ejercicio 1
longitud :: Integer->Integer
longitud n = longitudDesde n 1

longitudDesde :: Integer->Integer->Integer
longitudDesde 1 _  = 1
longitudDesde n k | mod n (nEsimoPrimo k) == 0 = longitudDesde (div n (nEsimoPrimo k)) k 
                  | otherwise = longitudDesde n (k+1) + 1

--Ejercicio 2
iesimo :: Integer->Integer->Integer
iesimo n i = qEsimaPotencia n (nEsimoPrimo i)

qEsimaPotencia :: Integer->Integer->Integer
qEsimaPotencia n q | (mod n q) == 0 = 1 + (qEsimaPotencia (div n q) q)
                   | otherwise = 0

--Ejercicio 3
headN :: Integer->Integer
headN n = iesimo n 2

--Ejercicio 4
--PD: Alto salame porque la función salía dividiendo por el número primo más grande elevado a su qEsimaPotencia.
tailN :: Integer->Integer
tailN n | l == 1 = 1
        | otherwise = (nEsimoPrimo (l-1)^(q)) * (tailN (div n ((nEsimoPrimo l)^q)))
         where 
         l = longitud n
         q = qEsimaPotencia n (nEsimoPrimo l)

--Ejercicio5
codificarALista :: Integer->[Integer]
codificarALista n = codificarALista' n 1

codificarALista' :: Integer->Integer->[Integer]
codificarALista' n p | longitud n == 1 = [q]
                     | otherwise = q : (codificarALista' (div n ((nEsimoPrimo p)^q))) (p+1)
         where 
         q = qEsimaPotencia n (nEsimoPrimo p)

--Ejercicio6
godelIncompleta :: [Integer]->Integer
godelIncompleta n = godelCompleta n 1

godelCompleta :: [Integer]->Integer->Integer
godelCompleta [] _ = 1
godelCompleta n p = (nEsimoPrimo p)^(head n) * godelCompleta (tail n) (p+1)
              