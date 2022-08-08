esMultiploDe3 :: Int -> Bool
esMultiploDe3 n | n == 1 || n == 2 = False
esMultiploDe3 3                    = True
esMultiploDe3 n                    = esMultiploDe3 (n-3)

sumaImpares :: Int -> Int
sumaImpares 1 = 1
sumaImpares n = (2*n-1) + sumaImpares (n-1) 

medioFact :: Int -> Int
medioFact 1 = 1
medioFact 2 = 2
medioFact n = n*medioFact (n-2)

sumaDigitos :: Int -> Int
sumaDigitos n | n <= 9 = n
sumaDigitos n          = mod n 10 + sumaDigitos (div n 10)

digitosIguales :: Int -> Bool
digitosIguales n | n < 100 && mod n 11 == 0 = True
digitosIguales n | n < 100 && mod n 11 /= 0 = False
digitosIguales n                            = digitosIguales (div n 10)