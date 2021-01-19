cantDigitos :: Int -> Int
cantDigitos 0 = 0
cantDigitos n = cantDigitos (n `div` 10) + 1

esBinario :: Int -> Bool
esBinario 0 = True
esBinario n = ((n `mod` 10) <= 1) && esBinario( n `div` 10)

tribonacci :: Int -> Int
tribonacci n | n<= 2 = n
             | otherwise = tribonacci(n-1) + tribonacci(n-2) + tribonacci(n-3)

multiplo3 :: Int->Bool
multiplo3 n | n<0 = False
            | n==0 = True
            | otherwise = multiplo3(n-3)

diabolico :: Int -> Bool
diabolico n | n>=10 = ((n `mod` 10) == t) && diabolico( n `div` 10)
            | otherwise = True
        where     
              t = (n `mod` 100 ) `div` 10
{-otra forma de hacerlo
diabolico n | n<10 = True
            | (n `mod` 10) == mod (div n 10) 10 = diabolico(div n 10)
            | otherwise = False
-}
potenciaOtro :: Int-> Int-> Bool
potenciaOtro n m = n==1 || potenciaOtro (n `div` m) m

potencia :: Int->Int->Int
potencia _ 0 = 1
potencia n m = n * (potencia n (m-1)) 

