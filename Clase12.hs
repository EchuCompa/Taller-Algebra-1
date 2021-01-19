module Clase12
where
import Clase7


mcd :: Integer -> Integer -> Integer
mcd a 0 = abs a
mcd a b = mcd b (mod a b)


type Polinomio = [Float]
type Monomio = (Float, Integer)


-- 1


limpiar :: [Float] -> Polinomio
limpiar (0:xs) = limpiar xs
limpiar p = p


grado :: Polinomio -> Integer
grado [] = undefined
grado [x] = 0
grado (x:xs) = 1 + grado xs


evaluar :: Polinomio -> Float -> Float
evaluar [] _ = 0
evaluar p x = ((head p) * (x^n)) + evaluar (limpiar (tail p)) x
    where n = grado p


-- 2


sumaAux :: Polinomio -> Polinomio -> [Float]
sumaAux [] p = p
sumaAux p [] = p
sumaAux p q = sumaAux (init p) (init q) ++ [(last p) + (last q)]


suma :: Polinomio -> Polinomio -> Polinomio
suma p q = limpiar (sumaAux p q)

-- 3


productoPorEscalar :: Float -> Polinomio -> Polinomio
productoPorEscalar 0 _ = []
productoPorEscalar x [] = []
productoPorEscalar x p = (x * head(p)) : (productoPorEscalar x (tail p))


resta :: Polinomio -> Polinomio -> Polinomio
resta p q = suma p (productoPorEscalar (-1) q)


productoPorMonomio :: Monomio -> Polinomio -> Polinomio
productoPorMonomio (a, 0) p = productoPorEscalar a p
productoPorMonomio (a, n) p = (productoPorMonomio (a, (n-1)) p) ++ [0]


producto :: Polinomio -> Polinomio -> Polinomio
producto [] q = []
producto p q = suma (productoPorMonomio (head p, grado p) q)  (producto (tail p) q)


-- 4


ceros :: Integer -> [Float]
ceros 0 = []
ceros n = 0: (ceros (n-1))

 
hacerPolinomio :: Monomio -> Polinomio
hacerPolinomio (a, n) = a : (ceros n)


derivadaMonomio :: Monomio -> Monomio
derivadaMonomio (a, n) = (a*fromIntegral(n), n-1)

derivada :: Polinomio->Polinomio
derivada [x] = []
derivada p = suma (hacerPolinomio (derivadaMonomio mono)) (derivada (tail p))
          where mono = (head p, grado p)

derivadaNEsima :: Integer->Polinomio->Polinomio
derivadaNEsima 0 p = p
derivadaNEsima n p = derivadaNEsima (n-1) (derivada p)
-- 5

primerCociente :: Polinomio -> Polinomio -> Monomio
primerCociente p q | grado p >= grado q = (head p / head q, grado p - grado q)


primerResto :: Polinomio -> Polinomio -> Polinomio
primerResto p q | grado p >= grado q = resta p (productoPorMonomio (primerCociente p q) q)


division :: Polinomio -> Polinomio -> (Polinomio, Polinomio)
division p [] = undefined
division [] q = ([],[])
division p  q | grado p < grado q = ([], p)
              | otherwise = (suma (hacerPolinomio (primerCociente p q)) c', r')
          where (c', r') = division (primerResto p q) q


-- 6


mcdPNM :: Polinomio -> Polinomio -> Polinomio
mcdPNM p [] = p
mcdPNM p q  = mcdPNM q (snd (division p q))


hacerMonico :: Polinomio -> Polinomio
hacerMonico p | p /= [] = productoPorEscalar (1 / (head p)) p


mcdP :: Polinomio -> Polinomio -> Polinomio
mcdP p q = hacerMonico (mcdPNM p q)

multiplicidad :: Float->Polinomio->Integer
multiplicidad x p | (evaluar p x) /= 0 = 0
                  | otherwise = multiplicidad x (derivada p) + 1

raicesMultiples :: Polinomio->Bool
raicesMultiples p = grado (mcdP p (derivada p)) >= 1

-- 7

type Racional = (Integer, Integer)


armaR :: Integer -> Integer -> Racional
armaR num den | den == 0 = undefined
              | den < 0 = armaR (-num) (-den)
              | otherwise = (div num d, div den d)
 where d = mcd num den              


sumaR :: Racional -> Racional -> Racional
sumaR (a, b) (c, d) = armaR (a*d+b*c) (b*d)


multiplicaR :: Racional -> Racional -> Racional
multiplicaR (a, b) (c, d) = armaR (a*c) (b*d) 


potenciaR :: Racional -> Integer -> Racional
potenciaR _ 0 = (1, 1)
potenciaR r n = multiplicaR (potenciaR r (n-1)) r


-- 8


type PolinomioZ = [Integer]


limpiarZ :: [Integer] -> PolinomioZ
limpiarZ (0:xs) = limpiarZ xs
limpiarZ p = p


gradoZ :: PolinomioZ -> Integer
gradoZ [] = undefined
gradoZ [x] = 0
gradoZ (x:xs) = 1 + gradoZ xs


evaluarZ :: PolinomioZ -> Racional -> Racional
evaluarZ [] _ = (0, 1)
evaluarZ p x =   sumaR (multiplicaR (head p, 1) (potenciaR x n))  (evaluarZ (limpiarZ (tail p)) x)
    where n = gradoZ p


esRaizRacional :: PolinomioZ -> Racional -> Bool
esRaizRacional p r = evaluarZ p r == (0,1)


raicesRacEnConjunto :: PolinomioZ -> Set Racional -> Set Racional
raicesRacEnConjunto p [] = []
raicesRacEnConjunto p (x:xs) | esRaizRacional p x = x : raicesRacEnConjunto p xs
                             | otherwise = raicesRacEnConjunto p xs
                    

-- 9


divisores :: Integer -> Set Integer
divisores n = divisoresHasta n (abs n)


divisoresHasta :: Integer -> Integer -> Set Integer
divisoresHasta n 1 = [1, -1]
divisoresHasta n k | mod n k == 0 = [k, -k] ++ divisoresHasta n (k-1)
                   | otherwise = divisoresHasta n (k-1)


divisoresPos :: Integer -> Set Integer
divisoresPos n = divisoresPosHasta n (abs n)


divisoresPosHasta :: Integer -> Integer -> Set Integer
divisoresPosHasta n 1 = [1]
divisoresPosHasta n k | mod n k == 0 = k:divisoresPosHasta n (k-1)
                      | otherwise = divisoresPosHasta n (k-1)


agregarRac :: Racional -> Set Racional -> Set Racional
agregarRac r c | elem r c = c
               | otherwise = r:c


armarSetRac :: Set (Integer, Integer) -> Set Racional
armarSetRac [] = []
armarSetRac ((num, den):xs) = agregarRac (armaR num den) (armarSetRac xs)


candidatosRaices :: PolinomioZ -> Set Racional
candidatosRaices [] = undefined
candidatosRaices p | last p == 0 = agregarRac (0,1) (candidatosRaices (init p))
                   | otherwise = armarSetRac (productoCartesiano (divisores (last p)) (divisoresPos (head p)))

raicesRacionales :: PolinomioZ -> Set Racional
raicesRacionales p = raicesRacEnConjunto p (candidatosRaices p)
