--Ejercicio 1

esMultiploDe3 :: Int -> Bool
esMultiploDe3 1 = False
esMultiploDe3 2 = False
esMultiploDe3 3 = True
esMultiploDe3 n = esMultiploDe3 (n-3)

--Ejercici 2

sumaImpares :: Int -> Int
sumaImpares 1 = 1
sumaImpares n = (2*n-1) + sumaImpares (n-1) 

--Ejercicio 3

medioFact :: Int -> Int
medioFact 1 = 1
medioFact 2 = 2
medioFact n = n*medioFact (n-2)

--Ejercicio 4

sumaDigitos :: Int -> Int
sumaDigitos n | n <= 9 = n
sumaDigitos n          = mod n 10 + sumaDigitos (div n 10)

--Ejercicio 5

digitosIguales :: Int -> Bool
digitosIguales n | n < 10                        = True
                 | mod n 10 == mod (div n 10) 10 = digitosIguales (div n 10)
                 | otherwise                     = False