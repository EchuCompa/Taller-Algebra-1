type Set a = [a]
--Funciones de la clase anterior
pertenece :: Int->Set Int->Bool
pertenece _ [] = False
pertenece x (y:ys) = x==y || pertenece x ys

incluido :: Set Int->Set Int->Bool
incluido [] y = True
incluido (x:xs) y = pertenece x y && incluido xs y 

perteneceC :: Set Int->Set (Set Int)->Bool
perteneceC _ [] = False
perteneceC xs (ys:yss) = iguales xs ys || perteneceC xs yss

iguales :: Set Int->Set Int->Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1

agregarC :: Set Int->Set (Set Int)->Set (Set Int)
agregarC xs xss | perteneceC xs xss = xss
                | otherwise = xs:xss

unionC :: Set (Set Int)->Set (Set Int)->Set (Set Int)
unionC [] ys = ys
unionC (x:xs) ys | perteneceC x ys = unionC xs ys
                 | otherwise = (x) : (unionC xs ys)

cardinal :: Set Int->Int
cardinal [] = 0
cardinal l = 1 + cardinal (tail l)

--Unión pero para las permutaciones
unionP :: Set (Set Int)->Set (Set Int)->Set (Set Int)
unionP [] ys = ys
unionP (x:xs) ys = x : (unionP xs ys)

--Acá empieza lo bueno

--permutaciones :: Set Int->Set (Set Int)
permutaciones [] = []
permutaciones (c:c1) = agregarXEnSubconjuntos c (permutaciones c1)

--Agrega a "x" en la posición "p" del conjunto "ys"
agregarEnPosicion :: Int->Int->Set Int->Set Int
agregarEnPosicion 1 x ys = x:ys
agregarEnPosicion p x (y:ys) = y: (agregarEnPosicion (p-1) x ys)

--Agrega a "x" en todas las posiciones menores o iguales a "p" del conjunto "c1" 
agregarNPosi :: Int->Int->Set Int->Set (Set Int)
agregarNPosi 0 _ _ = []
agregarNPosi _ x [] = [[x]]
agregarNPosi p x c1 = (agregarEnPosicion p x c1) : (agregarNPosi (p-1) x c1)


--EL PROBLEMA ESTÁ ACA, PORQUE NO DEVUELVE LO QUE DEBERÍA CON 1 [[2,3], [3,2]], ya no hay problema :)
--Agrega "x" en todos los subconjuntos en todas las posiciones posibles
agregarXEnSubconjuntos :: Int->Set (Set Int)->Set (Set Int)
agregarXEnSubconjuntos x [] = [[x]]
agregarXEnSubconjuntos x [c] = agregarNPosi (cardinal c + 1) x c
agregarXEnSubconjuntos x (c:c1) = unionP (agregarNPosi (cardinal c + 1) x c) (agregarXEnSubconjuntos x c1)



