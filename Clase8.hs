module Clase8
where
--------------------------------------------------------------------------------
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

type Set a = [a]

vacio :: Set a
vacio = []

agregar :: (Eq a) => a -> Set a -> Set a
agregar n c | n `elem` c = c
            | otherwise = n:c

union :: Eq a => Set a -> Set a -> Set a
union [] ys     = ys
union (x:xs) ys = union xs (agregar x ys)

cardinal :: Set Int->Int
cardinal [] = 0
cardinal l = 1 + cardinal (tail l)

quitar :: Int->[Int]->[Int]
quitar n xs | head xs == n = tail xs
            | otherwise = (head xs) : (quitar n (tail xs))
--------------------------------------------------------------------------------
---------------------------- Funciones Clase 8 ---------------------------------
--------------------------------------------------------------------------------
agregarElementoAdelante :: Int-> Set [Int] -> Set [Int]
agregarElementoAdelante _ [] = []
agregarElementoAdelante x (ys:yss) = agregar (x:ys) (agregarElementoAdelante x yss)

agregarElementosAListas :: Set Int -> Set [Int] -> Set [Int]
agregarElementosAListas [] _  = []
agregarElementosAListas (x:xs) c = union (agregarElementoAdelante x c) (agregarElementosAListas xs c)

variaciones :: Set Int-> Int-> Set [Int]
variaciones c 0 = [[]]
variaciones c k = agregarElementosAListas c (variaciones c (k-1))

---Continua la aventura
permutaciones :: Set Int-> Set [Int]
permutaciones [] = [[]]
permutaciones (c:c1) = insertarEnCadaPosDeTodasLasListas (permutaciones c1) c

--Agrega a "x" en la posición "p" del conjunto "ys"
insertarEn :: Int->Int->Set Int->Set Int
insertarEn 1 x ys = x:ys
insertarEn p x (y:ys) = y: (insertarEn (p-1) x ys)

--Agrega a "x" en todas las posiciones menores o iguales a "p" del conjunto "c1" 
insertarEnCadaPos :: [Int]->Int->Int->Set [Int]
insertarEnCadaPos c1 x 1 = agregar (insertarEn 1 x c1) []
insertarEnCadaPos c1 x p = agregar (insertarEn p x c1) (insertarEnCadaPos c1 x (p-1))

insertarEnCadaPosDeTodasLasListas :: Set [Int] -> Int -> Set [Int]
insertarEnCadaPosDeTodasLasListas [] c = []
insertarEnCadaPosDeTodasLasListas (xs:xss) c = union (insertarEnCadaPos xs c (length xs +1)) (insertarEnCadaPosDeTodasLasListas xss c)

insertarEn1TodasLasListas :: Int->Set [Int]->Set [Int]
insertarEn1TodasLasListas x [] = []
insertarEn1TodasLasListas x (c:c1) = (x:c) : (insertarEn1TodasLasListas x c1)
--------------------------------------------------------------------------------
---------------------------- Ejercicios de Tarea Clase 8 -----------------------
--------------------------------------------------------------------------------

--Ejercicio 1 
lista :: Int->Set Int
lista 0 = []
lista n = n : (lista (n-1))


bolitasEnCaja :: Int -> Int -> Set [Int]
bolitasEnCaja n k = variaciones (lista k) n

--Ejercicio 2 
bolitasEnCaja' :: Int->Int-> Set [Int]
bolitasEnCaja' n k  = insertarEn1TodasLasListas  1 (variaciones (quitar 1 (lista k)) n) 

--Ejercicio 3 
--Modo súper ineficiente, pero súper sencillo
estaOrdenada :: Set Int ->Bool
estaOrdenada [l1] = True
estaOrdenada (l1:l2:l) = l1<l2 && estaOrdenada (l2:l)

devuelveOrdenados ::  Set [Int]-> Set [Int]
devuelveOrdenados [] = []
devuelveOrdenados (c:c1) | estaOrdenada c = c: devuelveOrdenados c1
                         | otherwise = devuelveOrdenados c1

listasOrdenadas :: Int->Int->Set [Int]
listasOrdenadas k n = devuelveOrdenados (variaciones (lista n) k)


--Ejercicio 4 vilmente replicado
sucesionAB :: Int -> Int -> Set String
sucesionAB 0 0 = [[]]
sucesionAB 0 b = [replicate b 'b']
sucesionAB a b = insertarXss 'a' (sucesionAB (a-1) b) 


insertar :: a -> Int -> [a] -> [a]
insertar n i [] = [n]
insertar n 1 xs = n:xs
insertar n i (x:xs) = x:(insertar n (i-1) xs)

insertarXss :: (Eq a) => a -> Set [a] -> Set [a]
insertarXss a [] = [] 
insertarXss a (xs:xss) = (insertarXsDesde a (length xs + 1) xs) `union` (a `insertarXss` xss) 

insertarXsDesde :: (Eq a) => a -> Int -> [a] -> [[a]]
insertarXsDesde _ 0 _ = [] 
insertarXsDesde a n xs = (insertar a (n) xs) `agregar` (insertarXsDesde a (n-1) xs)

--Ejercicio5
sucesionABC :: Int -> Int -> Int-> Set String
sucesionABC a b 0 = sucesionAB a b 
sucesionABC a b c = insertarXss 'c' (sucesionABC a b (c-1))

--Ejercicio6
subconjuntos ::  Set Int-> Int -> Set (Set Int)
subconjuntos [] _ = [] 
subconjuntos xs 1 = dividir (xs)
subconjuntos (x:xs) k = union (insertarEn1TodasLasListas x (subconjuntos xs (k-1))) (subconjuntos xs k)

dividir :: [Int]-> Set [Int]
dividir [] = []
dividir (x:xs) = [x] : (dividir xs) 

{-

---Horas desperdiciadas en algo que sigo sin saber porque no funciona, yeyy
--Ejercicio6
--Inserta un elemento "a" en todos los conjuntos de "xs" en los cuáles "a" no esta y la lista es de longitud menor a "l"
insertarNoRepe :: Int-> Set (Set Int)-> Int-> Set (Set Int)
insertarNoRepe a [] l = [] 
insertarNoRepe a (xs:xss) l | (length xs)>l  = (insertarNoRepe a xss l)
                            | otherwise  = agregar (agregar a xs) (insertarNoRepe a xss l) 
---ACA ESTA EL ERROR, PORQUE HAY ALGO QUE PARECIERA QUE NO ESTÁ AGREGANDO

insertarNoRepe 1 [[1],[2],[3]] = [[1],[2,1],[3,1]]
insertarNoRepe 4  [[1],[2,1],[3,1]] = [[1,4],[2,1],[3,1]]


--Inserta una lista de elementos en otra lista
insertarEleLista :: Set Int-> Set [Int]-> Int-> Set [Int]
insertarEleLista [] xss _ = xss
insertarEleLista eles xss l = insertarNoRepe (head eles) (insertarEleLista (tail eles) xss l) l

insertarEleLista [1,2,3,4] 3=  [[1],[2],[3],[4]]


--Separa a un conjunto en un múltiples conjuntos
dividir :: [Int]-> Set [Int]
dividir [] = []
dividir (x:xs) = [x] : (dividir xs) 
dividir [1,2,3,4] = [[1],[2],[3],[4]]

subconjuntos :: Set Int-> Int -> Set (Set Int)
subconjuntos [] _ = []
subconjuntos  _ 0 = []
subconjuntos xs k = insertarEleLista xs (dividir xs) k 

--Ejercicio 4
--Mi idea sería recrear la función permutaciones pero esta vez para Char
insertarEn' :: Int->[Char]->[Char]->[Char]
insertarEn' 1 x ys = x ++ ys
insertarEn' p x (ws) = [(head ws)] ++ (insertarEn' (p-1) x (tail ws)) 

--Agrega a "x" en todas las posiciones menores o iguales a "p" de la palabra "w1" 
insertarEn2' :: Int->[Char]->[Char]->Set [Char]
insertarEn2' 1 x w1 = agregar (insertarEn' 1 x w1) []
insertarEn2' p x w1   = agregar (insertarEn' p x w1) (insertarEn2' (p-1) x w1)

--Inserta la letra "c" en todas las posiciones de todas las palabras. 
insertarEn3' :: Set [Char] -> [Char] -> Set [Char]
insertarEn3' [] c = []
insertarEn3' w1 c = unionChar (insertarEn2' (length c + 1) c (head w1) ) (insertarEn3' (tail w1) c)

mismaPalabra :: [Char] -> [Char] -> Bool
mismaPalabra [x] y = x == (head y)
mismaPalabra x [y] = (head x) == (y)
mismaPalabra (x:xs) (y:ys) = x==y && mismaPalabra xs ys

--Comprueba si una palabra está dentro de una lista de palabras
incluido :: [Char] -> Set [Char]->Bool
incluido w1 [] = False
incluido w1 c1 = (mismaPalabra (head c1) w1) || (incluido w1 (tail c1))

--Une dos listas de palabras
unionChar :: Set [Char] -> Set [Char] -> Set [Char]
unionChar w1 [] = w1
unionChar w1 j2 | (incluido (head j2) w1) = unionChar w1 (tail j2)
                | otherwise = (head j2) : (unionChar w1 (tail j2))

--La tan esperada permutaciones
permutacionesChar :: Set [Char]-> Set [Char]
permutacionesChar [] = [[]]
permutacionesChar w1 = insertarEn3' (permutacionesChar (tail w1)) (head w1)

-}