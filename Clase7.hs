module Clase7
where
type Set a = [a]

--Funciones creadas para trabajar con conjuntos creadas en clase
pertenece :: Integer->Set Integer->Bool
pertenece _ [] = False
pertenece x (y:ys) = x==y || pertenece x ys

agregar :: Integer->Set Integer->Set Integer
agregar x xs | pertenece x xs = xs
             | otherwise = x:xs

cardinal :: Set Integer->Integer
cardinal [] = 0
cardinal l = 1 + cardinal (tail l)

incluido :: Set Integer->Set Integer->Bool
incluido [] y = True
incluido (x:xs) y = pertenece x y && incluido xs y 

iguales :: Set Integer->Set Integer->Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1

union :: Set Integer->Set Integer->Set Integer
union [] ys = ys
union (x:xs) ys | incluido (x:xs) ys = ys
                | pertenece x ys = union xs ys
                | otherwise = (x) : (union xs ys)

interseccion :: Set Integer->Set Integer->Set Integer
interseccion [] _ = []
interseccion (x:xs) c2 | pertenece x c2 = x : (interseccion xs c2)
                       | otherwise = interseccion xs c2

diferencia :: Set Integer->Set Integer->Set Integer
diferencia [] _ = []
diferencia (x:xs) c2 | pertenece x c2 = diferencia xs c2
                     | otherwise = x: (diferencia xs c2)

diferenciaSimetrica :: Set Integer->Set Integer->Set Integer
diferenciaSimetrica a b = (union a b) `diferencia` (interseccion a b)

---Modificaciones a las funciones para poder trabajar con pertenencia de conjuntos a otros conjuntos
perteneceC :: Set Integer->Set (Set Integer)->Bool
perteneceC _ [] = False
perteneceC xs (ys:yss) = iguales xs ys || perteneceC xs yss

agregarC :: Set Integer->Set (Set Integer)->Set (Set Integer)
agregarC xs xss | perteneceC xs xss = xss
                | otherwise = xs:xss

unionC :: Set (Set Integer)->Set (Set Integer)->Set (Set Integer)
unionC [] ys = ys
unionC (x:xs) ys | perteneceC x ys = unionC xs ys
                 | otherwise = (x) : (unionC xs ys)

agregarATodos :: Integer->Set (Set Integer)->Set (Set Integer)
agregarATodos x [] = []
agregarATodos c1 (c:c2) = agregarC (agregar c1 c) (agregarATodos c1 c2) 

---Función partes hecha en clase
partes :: Set Integer-> Set (Set Integer)
partes [] = []
partes (x:xs) = unionC (partes xs) (agregarATodos x (partes xs))


---ProductoCartesiano
---Agrega "x" a todos los subconjuntos de "(y:ys)"
agregarTruchi :: Integer-> Set Integer ->Set (Integer, Integer)
agregarTruchi x [] = []
agregarTruchi x (y:ys) = (x,y) : (agregarTruchi x ys)

productoCartesiano :: Set Integer->Set Integer->Set (Integer, Integer)
productoCartesiano [] _ = []
productoCartesiano (x:xs) ys = (agregarTruchi x ys) ++ (productoCartesiano xs ys)

--Reemplaze todos los "Set (Set Integer)" por "Set (Integer, Integer)" por los polinomios, tal vez arruine algo