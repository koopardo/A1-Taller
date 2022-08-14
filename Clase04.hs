--suma: suma de los primeros "n" naturales

suma :: Int -> Int
suma 1 = 1
suma n = n + suma (n-1)

f1 :: Int -> Int
f1 0 = 1
f1 n = 2^n + f1 (n-1)

f2 :: Int -> Float -> Float
f2 1 q = q
f2 n q = q^n + f2 (n-1) q

f3 ::  Int -> Float -> Float
f3 0 q = 0
f3 1 q = q + q^2
f3 n q = q^(2*n) + q^(2*n-1) + f3 (n-1) q

f4 ::  Int -> Float -> Float
f4 0 q = 0
f4 1 q = q + q^2
f4 n q = -q^(n-1) + q^(2*n) + q^(2*n-1) + f4 (n-1) q

factorial :: Int -> Int
factorial 0 = 1
factorial n = n*factorial (n-1)

eAprox :: Int -> Float
eAprox 0 = 1
eAprox n = (1/fromIntegral (factorial n)) + eAprox (n-1)

e :: Float
e = eAprox 10

-- Ejercicio 1

dobleSuma :: Int -> Int -> Int
dobleSuma 1 m = m
dobleSuma n m = dobleSuma (n-1) m + sumaBaseFija m n

--sumaBaseFija: Suma de q^i, con i = 1,...,n y "q" entero.

sumaBaseFija :: Int -> Int -> Int
sumaBaseFija 0 q = 0
sumaBaseFija 1 q = q
sumaBaseFija n q = q^n + sumaBaseFija (n-1) q

--sumaExponenteFijo: Suma de i^m, i = 1,...,n y "m" entero.

sumaExponenteFijo :: Int -> Int -> Int
sumaExponenteFijo 1 m = 1
sumaExponenteFijo n m = n^m + sumaExponenteFijo (n-1) m

dobleSuma2 :: Int -> Int -> Int
dobleSuma2 n 1 = div (n*(n+1)) 2
dobleSuma2 n m = dobleSuma2 n (m-1) + sumaExponenteFijo n m

dobleSuma3 :: Int -> Int -> Int
dobleSuma3 n 1 = div (n*(n+1)) 2
dobleSuma3 1 m = m
dobleSuma3 n m = dobleSuma3 (n-1) (m-1) + sumaBaseFija m n + sumaExponenteFijo n m - n^m

--Ejercicio 2

sumaPotencias :: Float -> Int -> Int -> Float
sumaPotencias q 1 m = q*(f2 m q)
sumaPotencias q n m = (q^n)*(f2 m q) + sumaPotencias q (n-1) m

--Ejercicio 3

sumaRacionales :: Int -> Int -> Float
sumaRacionales n 1 = fromIntegral (suma n) 
sumaRacionales n m = (1/fromIntegral m)*fromIntegral (suma n) + sumaRacionales n (m-1)

--Ejercicio 4

g1 :: Int -> Int -> Int
g1 1 1 = 1
g1 i n = sumaBaseFija n i - sumaBaseFija (i-1) i

--Ejercicio 5

g2 :: Int -> Int
g2 1 = 1
g2 n = sumaExponenteFijo n n + g2 (n-1)

--Ejercicio 6

g3 :: Int -> Int
g3 1 = 4
g3 n = 2^(2*n) + g3(n-1)

--Ejercicio 7

--f: dado "n", suma los números naturales que tienen los digitos iguales hasta "n" ("n" inclusive).

f :: Int -> Int
f 1 = 1
f n | digitosIguales n == True  = n + f(n-1)
f n | digitosIguales n == False = f(n-1)

digitosIguales :: Int -> Bool
digitosIguales n | n < 0                    = undefined
digitosIguales n | n < 10                   = True
digitosIguales n | n < 100 && mod n 11 == 0 = True
digitosIguales n | n < 100 && mod n 11 /= 0 = False
digitosIguales n                            = digitosIguales (div n 10)
