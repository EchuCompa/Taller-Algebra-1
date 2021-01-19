module Clase6
where
--Determina la longitud de una lista, no le pongo el tipo para poder 
--usarla en "zipi", sino no sabía cómo solucionar el error que me tiraba
--longitud :: [Int]->Int
longitud [] = 0
longitud l = 1 + longitud (tail l)

productoria :: [Int]->Int
productoria l | l == [] = 1
              | otherwise = (head l) * productoria (tail l)

sumarN :: Int->[Int]->[Int]
sumarN n xs | xs == [] = []
            | otherwise =  (head xs + n) : (sumarN n (tail xs))

sumarElPrimero :: [Int]->[Int]
sumarElPrimero xs  = (x*2):sumarN x (tail xs)
                   where x = head xs

--Agarra el último elemento de una lista
ultimo :: [Int]->Int
ultimo [x] = x
ultimo xs = ultimo (tail xs)
 
sumarElUltimo :: [Int]->[Int]
sumarElUltimo xs = sumarN (ultimo xs) xs

pares :: [Int] -> [Int]
pares xs | xs == [] = []
         | (head xs `mod` 2) == 0 = (head xs) : (pares (tail xs))
         | otherwise = pares (tail xs)

quitar :: Int->[Int]->[Int]
quitar n xs | head xs == n = tail xs
            | otherwise = (head xs) : (quitar n (tail xs))

quitartodas :: Int->[Int]->[Int]
quitartodas n xs | xs == [] = []
                 | head xs == n = quitartodas n (tail xs)
                 | otherwise = (head xs) : (quitartodas n (tail xs))

--Función auxiliar que busca si el primer elemento de la lista esta repetido.
primerEleRepe :: [Int]->Bool
primerEleRepe xs | longitud xs == 1 = False
                 | head xs == head (tail xs) = True
                 | otherwise = primerEleRepe ((head xs) :(tail (tail xs)))

--Va buscando si el primer, segundo, .... enesimo esta repetido. Por lo que
--al final va a terminar revisando a todos. Ineficiente pero cumple el trabajo.
hayRepetidos :: [Int]->Bool
hayRepetidos [] = False
hayRepetidos xs | primerEleRepe xs = True
                | otherwise = hayRepetidos (tail xs)

eliminarRepetidosAlFinal :: [Int]->[Int]
eliminarRepetidosAlFinal xs | (hayRepetidos xs) == False = xs
                            | otherwise = (head xs) : (eliminarRepetidosAlFinal (quitartodas (head xs) (tail xs)))

eliminarRepetidosAlInicio :: [Int]->[Int]
eliminarRepetidosAlInicio xs | hayRepetidos xs == False = xs
                             | primerEleRepe xs = eliminarRepetidosAlInicio (tail xs)
                             | otherwise = (head xs) : (eliminarRepetidosAlInicio (tail xs))

maximo :: [Int]->Int
maximo [x] = [x]
maximo (x:xs) | x > head xs = maximo (x : (tail xs))
              | otherwise = maximo xs

--Se que esto es tremendamente ineficiente pero fue la forma más sencilla que se me ocurrió
--aunque google algunos algoritmos de ordenamiento y no sabría cómo aplicarlo sin agregar
--más variables o una función auxiliar
ordenar :: [Int]->[Int]
ordenar xs | xs == [] = []
           | otherwise = (max) : (ordenar (quitar max xs))
           where max = maximo xs

--Quita el último elemento de una lista
quitaUltimo :: [Int]->[Int]
quitaUltimo [x] = []
quitaUltimo xs = (head xs) : (quitaUltimo (tail xs))

reverso :: [Int]->[Int]
reverso [] = []
reverso xs = (ultimo xs) : (reverso (quitaUltimo xs))

concatenar :: [Int]->[Int]->[Int] 
concatenar [] l2 = l2
concatenar l1 l2 = concatenar (quitaUltimo l1) ((ultimo l1):(l2)) 

zipi :: [a]->[b]->[(a,b)]
zipi [] _ = []
zipi _ [] = []
zipi (l1:l11) (l2:l22)  = ((l1) ,(l2)) : (zipi (l11) (l22))



