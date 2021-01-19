module Naturales
where
     suma :: Int-> Int-> Int
     suma x 0 = x
     suma x y = suma (succ x) (pred y)

     mult :: Int->Int->Int
     mult x 0 = 0
     mult x y = suma x (mult x (pred y))

     resta :: Int->Int->Int
     resta x 0 = x
     resta x y = resta (pred x) (pred y)

     menor :: Int-> Int-> Bool
     menor n m = resta n m < 0

     mayor :: Int-> Int-> Bool
     mayor n m = not (menor n m)

     iguales :: Int->Int->Bool
     iguales n m = resta n m == 0
 