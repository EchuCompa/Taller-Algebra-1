estanRelacionados x y | s = True
                      | v && (x>3 && y>3) = True
                      | x>7 && y>7 = True 
                      | otherwise = False   
                where 
                     s = x<=3 && y<=3
                     v = x <= 7 && y <= 7

prodint :: Num a => (a,a) -> (a,a) -> a
prodint (x,y) (x1,y1) = x*x1 + y*y1

todoMenor :: Ord a => (a,a) -> (a,a) -> Bool
todoMenor (x,y) (x1,y1) = x<x1 && y<y1

distanciaPuntos :: Floating a => (a,a) -> (a,a) -> a
distanciaPuntos (x,y) (x1,y1) = sqrt ((x-x1)**2 + (y-y1)**2)

sumaTerna :: (Int,Int,Int) -> Int
sumaTerna (x,y,z) = x+y+z

posicPrimerPar :: (Int,Int,Int) -> Int
posicPrimerPar (x,y,z) | mod x 2 == 0 = 1
                       | mod y 2 == 0 = 2
                       | mod z 2 == 0 = 3
                       | otherwise = 4

crearPar :: p -> p1 -> (p,p1)
crearPar x y = (x,y)

invertir :: (a,b) -> (b,a)
invertir (x,y) = (y,x)