tdr 0 = []
tdr n | ( mod (5^n) (13) ) == 1 = (n) : (tdr (n-1))
      | otherwise = (tdr (n-1))

fact 0 = 1
fact n = n * (fact (n-1))