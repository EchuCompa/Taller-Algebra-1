module Clase1
where
abso :: Int -> Int 
abso x0 | x0 >= 0 = x0
       | otherwise = (-x0)
   
maxAbso :: Int -> Int -> Int
maxAbso x1 y1 | (abso x1) > (abso y1) = abso x1
            | otherwise = abso y1

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z = max (max x y) z 
             

--La función sin pattern matching
-- algu0 :: Float -> Float -> String
-- algu0 x y | x==0 = show x ++ " es 0 por si no lo notaste"
          -- | y==0 = show y ++ " es 0 por si no lo notaste" 
          -- | otherwise = "Ninguno es 0 por si no lo notaste" 
algu0 :: Float -> Float -> Bool
algu0 0 _ = True
algu0 _ 0 = True
algu0 _ _ = False

--La función sin pattern matching
--ambos0 :: Float -> Float -> String
--ambos0 x y | x==0 && y ==0 = show x ++ " y " ++ show y ++ "son 0"
--           | otherwise = "Siento decepcionarte, ambos no son 0"
ambos0 :: Float -> Float -> Bool
ambos0 0 _ = True
ambos0 _ 0 = True
ambos0 _ _ = False


esMulti :: Int -> Int -> String
esMulti x y | mod x y == 0 = "Correcto! " ++ show x ++ " es multiplo de " ++ show y
            | otherwise = "Incorrecto! " ++ show x ++ " no es multiplo de " ++ show y


digiUni:: Int -> Int
digiUni x = mod x 10

digiDec:: Int -> Int
digiDec x = div (mod x 100) 10
