module Clase11
where
type Complejo = (Float, Float)

re :: Complejo->Float
re (a,_) = a

im :: Complejo->Float
im (_,a) = a

conjugado :: Complejo->Complejo
conjugado (a,b) = (a,-b)

suma :: Complejo->Complejo->Complejo
suma (a,b) (c,d) = (a+b, c+d)

resta :: Complejo->Complejo->Complejo
resta (a,b) (c,d) = (a-b, c-d)

producto :: Complejo->Complejo->Complejo
producto (a,b) (c,d) = (a*c-b*d, a*d+b*c)

inverso :: Complejo->Complejo
inverso (a,b) = (a/(a^2 +b^2), -b/(a^2+b^2))

cociente :: Complejo->Complejo->Complejo
cociente a b = (re mod2 / re prod , im mod2 / re prod)
             where mod2 = producto b (conjugado b)
                   prod = producto a (conjugado b)

potencia :: Complejo->Integer->Complejo
potencia a 1 = a
potencia a n = producto a (potencia a (n-1))

cuadrati :: Float->Float->Float->(Complejo, Complejo)
cuadrati a b c | res>=0 = ( ((-b+sqrt (res))/(2*a),0) , ((-b-sqrt (res))/(2*a),0) )
               | res<=0 = ( (-b/(2*a),sqrt (-res)/2*a), (-b/(2*a),- (sqrt (-res)/(2*a))) )
                     where res = b^2-4*a*c

modulo :: Complejo -> Float
modulo (a,b) = sqrt (a^2+b^2)

argumento :: Complejo -> Float 
argumento (a,b) |cuadrante (a,b) == 1 = atan (b/a)
                |cuadrante (a,b) == 2 = pi + (atan (b/a))
                |cuadrante (a,b) == 3 = pi + (atan (b/a))
                |otherwise = 2*pi + (atan (b/a))

cuadrante :: Complejo -> Int
cuadrante (a,b) |a>=0 && b>=0 = 1
                |a<=0 && b>=0 = 2
                |a<=0 && b<=0 = 3
                |a>=0 && b<=0 = 4

pasarACartesianas ::  Float -> Float -> Complejo
pasarACartesianas r o =  (cos o * r, sin o * r)

raizCuadrada :: Complejo -> (Complejo, Complejo)
raizCuadrada a = ( (pasarACartesianas (sqrt r) (o/2)) , (pasarACartesianas (-r) (o/2)) ) 
              where (r,o) = (modulo a, argumento a)

cuadratiComplejo :: Complejo -> Complejo -> Complejo -> (Complejo,Complejo)
cuadratiComplejo a b c = ((cociente (suma bb raiz1) a2) , (cociente (suma bb raiz2) a2))
                    where b2 = producto b b
                          (raiz1, raiz2) = raizCuadrada (resta b2 (producto (4,0) (producto a c)))
                          bb = producto (-1,0) b
                          a2 = producto a (2,0)


raicesNEsimas :: Integer-> [Complejo]
raicesNEsimas n = raicesNEsimasDesde n n 

--Devuelve una cantidad k de raíces n-ésimas de la unidad.
raicesNEsimasDesde :: Integer -> Integer -> [Complejo]
raicesNEsimasDesde n 0 = []
raicesNEsimasDesde n k = (pasarACartesianas 1 (2*pi*(fromInteger k) /(fromInteger n))) : (raicesNEsimasDesde n (k-1))

--Devuelve la lista con las potencias 0, 1, . . . , n − 1 de la raız n-esima asociada a k
potenciasRaizNEsima :: Integer -> Integer -> [Complejo]
potenciasRaizNEsima n k = listaPotenciasHasta (agarrarElemento (raicesNEsimas n) k) n

--Devuelve el elemento en la posición k de la lista, comienza desde el 0
agarrarElemento :: [a]-> Integer-> a
agarrarElemento l 0 = head l
agarrarElemento (l1:l) k = agarrarElemento l (k-1) 

listaPotenciasHasta :: Complejo->Integer->[Complejo]
listaPotenciasHasta _ 0 = [(1,0)]
listaPotenciasHasta a n = (potencia a n) : (listaPotenciasHasta a (n-1))

