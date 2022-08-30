--Ejercicio 1

--sumaDivisores: suma los divisores de n hasta el número k (k y n naturales, k < n)

sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n 1                = 1
sumaDivisoresHasta n k | mod n k == 0 = k + sumaDivisoresHasta n (k-1)
sumaDivisoresHasta n k | mod n k /= 0 = sumaDivisoresHasta n (k-1)

--Ejercicio 2

--sumaDivisores: suma todos los divisores positivos de un natural n

sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n

--sumaDivisoresDesde: suma todos los divisores positivos de n desde un número k (k y n naturales, k > n)

sumaDivisoresDesde :: Int -> Int -> Int
sumaDivisoresDesde n k | k == n       = n
sumaDivisoresDesde n k | mod n k == 0 = k + sumaDivisoresDesde n (k+1)
sumaDivisoresDesde n k | otherwise    = sumaDivisoresDesde n (k+1)

--Ejercicio 3

--menorDivisorDesde: desde k hasta n, elige el menor divisor de n entre esos números (k y n naturales)

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k | k == n       = n
menorDivisorDesde n k | mod n k == 0 = k
menorDivisorDesde n k | otherwise    = menorDivisorDesde n (k+1)

--menorDivisor: nos da el menor divisor de un número natural n (que no sea 1)

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

--Ejercicio 4

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = menorDivisor n == n

esPrimo2 :: Int -> Bool
esPrimo2 n = sumaDivisores n == n+1

--Ejercicio 5

--nEsimoPrimo: devuelve el n-esimo primo (el primer primo es el 2, el segundo el 3, etc)

nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n-1))

--minimoPrimoDesde: nos da el primer primo desde n

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n  = n
minimoPrimoDesde n | otherwise  = minimoPrimoDesde (n+1)

--proximoPrimoDesde: nos da el proximo primo desde n

proximoPrimoDesde :: Int -> Int
proximoPrimoDesde n | esPrimo (n+1)  = n+1
proximoPrimoDesde n | otherwise      = proximoPrimoDesde (n+1)

--mEsimoPrimo: devuelve el m-esimo primo

mEsimoPrimo :: Int -> Int
mEsimoPrimo 1 = 2
mEsimoPrimo m = proximoPrimoDesde (mEsimoPrimo (m-1))

--Ejercicio 6

--menorFactDesde: menor factorial tal que m =< k!

menorFactDesde :: Int -> Int
menorFactDesde m = menorFactDesdeDesde 1 m

--menorFactDesdeDesde: menor factorial tal que i =< k y m =< k! 

menorFactDesdeDesde :: Int -> Int -> Int
menorFactDesdeDesde i m | factorial i >= m = factorial i
menorFactDesdeDesde i m | factorial i < m  = menorFactDesdeDesde (i+1) m

factorial :: Int -> Int
factorial 0 = 1
factorial n = n*factorial (n-1) 

--Ejercicio 7

--mayorFactHasta: devuelve el mayor factorial hasta m (incluido)

mayorFactHasta :: Int -> Int
mayorFactHasta m = mayorFactHastaHasta 1 m

--mayorFactHastaHasta: devuelve el mayor factorial desde i hasta m (incluido)

mayorFactHastaHasta :: Int -> Int -> Int
mayorFactHastaHasta i m | factorial i == m = m
mayorFactHastaHasta i m | factorial i > m  = div (factorial i) i
mayorFactHastaHasta i m | factorial i < m  = mayorFactHastaHasta (i+1) m

--Ejercicio 8

--esFact: dado un n, nos dice si es el factorial de algun número natural

esFact :: Int -> Bool
esFact n = mod (mayorFactHasta n) n == 0

--Ejercicio 9

--esFibonacci: nos dice si un número n pertenece a la sucesión de Fibonacci

esFibonacci :: Int -> Bool
esFibonacci n = hayFibonacciDesde 0 n

--hayFibonacciDesde: nos dice si desde m hasta n hay algun número de la sucesión de Fibonacci igual a n

hayFibonacciDesde :: Int -> Int -> Bool
hayFibonacciDesde m n | fibonacci m > n  = False
hayFibonacciDesde m n | fibonacci m == n = True 
hayFibonacciDesde m n | otherwise        = hayFibonacciDesde (m+1) n

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1)

--Ejercicio 10

--esSumaInicialDePrimos: nos dice si n es la suma de los primeros m primos (Ej: 5 es la suma de 2 y 3. Por otro lado, 6 no es la suma de 2, 3 o 5)

esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n = esSumaInicialDePrimosDesde 1 n 

--sumaDePrimosDesde: nos devuelve la suma de los primos entre m y n (incluidos y m <= n)

sumaDePrimosDesde :: Int -> Int -> Int
sumaDePrimosDesde m n | m > n     = 0
sumaDePrimosDesde m n | esPrimo m = m + sumaDePrimosDesde (m+1) n
sumaDePrimosDesde m n | otherwise = sumaDePrimosDesde (m+1) n

--esSumaInicialDePrimosDesde: nos dice si n es la suma de los primos en el intervalo m y n (incluidos)

esSumaInicialDePrimosDesde :: Int -> Int -> Bool
esSumaInicialDePrimosDesde m n | m > n                      = False
esSumaInicialDePrimosDesde m n | sumaDePrimosDesde m n == n = True
esSumaInicialDePrimosDesde m n | otherwise                  = esSumaInicialDePrimosDesde (m+1) n

--Ejercicio 11

--tomaValorMax: nos devuelve i perteneciente al intervalo [a,b] tal que sumaDivisores en i es máximo

tomaValorMax :: Int -> Int -> Int
tomaValorMax a b | a == b                             = a                  
tomaValorMax a b | sumaDivisores a > sumaDivisores b  = tomaValorMax a (b-1)
tomaValorMax a b | sumaDivisores a <= sumaDivisores b = tomaValorMax (a+1) b

--tomaValorMin: nos devuelve i perteneciente al intervalo [a,b] tal que sumaDivisores en i es mínimo


tomaValorMin :: Int -> Int -> Int
tomaValorMin a b | a == b                             = a                  
tomaValorMin a b | sumaDivisores a < sumaDivisores b  = tomaValorMin a (b-1)
tomaValorMin a b | sumaDivisores a >= sumaDivisores b = tomaValorMin (a+1) b