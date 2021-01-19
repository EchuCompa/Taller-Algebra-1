g1 :: Int->Int->Int
g1 i n | i==n = i^n
       | otherwise = i^n + g1 i (n-1)


---Ahora no esta mal
g2 :: Int->Int
g2 0 = 0
g2 n = h1 n n + g2 (n-1)


h1 :: Int -> Int -> Int
h1 i 0 = 0
h1 i n = n^i + h1 i (n-1)


{-Otra forma de hacer las cosas
g2' :: Int->Int
g2' n = h1' 1 n

h1' :: Int->Int->Int
h1' m n | n==m = n^m
        | otherwise = g1 m n + h1' (m+1) n
-}

g3 :: Int->Int
g3 0 = 0
g3 n | n `mod` 2 == 0 = 2^n + g3 (n-1)
     | otherwise = g3 (n-1)



g4 :: Int->Int
g4 0 = 0
g4 n | diabolico n = n + g4 (n-1)
     | otherwise = g4 (n-1)


diabolico :: Int -> Bool
diabolico n | n>=10 = ((n `mod` 10) == t) && diabolico( n `div` 10)
            | otherwise = True
        where
              t = (n `mod` 100 ) `div` 10
