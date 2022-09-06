--Ejercicio 1

productoria :: [Int] -> Int
productoria []     = 0
productoria (x:xs) = x*productoria xs

--Ejercicio 2

--sumarN: dado un n y una lista xs, suma n a cada elemento de xs

sumarN :: Int -> [Int] -> [Int]
sumarN n []     = []
sumarN n (x:xs) = (n+x):sumarN n xs

--Ejerciocio 3

--sumarElPrimero: dada una lista xs (no vacía), suma el primer elemento a cada elemento de la lista

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero (x:xs) = sumarN x (x:xs)

--Ejercicio 4

--sumarElUltimo: dada una lista xs (no vacía), suma el último elemento a cada elemento de la lista

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo l = sumarN (devuelveUltimo l) l

--devuelveUltimo: devuelve el último elemento de la lista

devuelveUltimo :: [Int] -> Int
devuelveUltimo [n]    = n
devuelveUltimo (x:xs) = devuelveUltimo xs

--Ejercicio 5

--pares: devuelve los elementos pares de una lista

pares :: [Int] -> [Int]
pares []                    = []
pares (x:xs) | mod x 2 == 0 = x:pares xs
pares (x:xs) | otherwise    = pares xs

--Ejercicio 6

--multiplosDeN: dado N y una lista, devuelve una lista con los elementos que son multiplos de N

multiplosDeN :: Int -> [Int] -> [Int]
multiplosDeN n []                    = []
multiplosDeN n (x:xs) | mod x n == 0 = x:multiplosDeN n xs
multiplosDeN n (x:xs) | otherwise    = multiplosDeN n xs

--Ejercicio 7

--quitar: dado n, elimina la primera aparición del elemento n en la lista

quitar :: Int -> [Int] -> [Int]
quitar n []                 = []
quitar n (x:xs) | x == n    = xs
quitar n (x:xs) | otherwise = x:quitar n xs 

--Ejercio 8

--hayRepetidos: nos dice si hay algun elemento repetido en la lista

hayRepetidos :: [Int] -> Bool
hayRepetidos [x]                             = False
hayRepetidos (x:xs) | primeroSeRepite (x:xs) = True
hayRepetidos (x:xs) | otherwise              = hayRepetidos xs

--primeroSeRepite: nos dice si el primer elemento de la lista se repite

primeroSeRepite :: [Int] -> Bool
primeroSeRepite [n]                   = False 
primeroSeRepite (x:xs) | x == head xs = True
primeroSeRepite (x:xs) | otherwise    = primeroSeRepite (quitar (head xs) (x:xs))

--eliminarRepetidos: elimina los elementos repetidos de una lista (elimina los elementos repetidos con menor posición en la lista) 

eliminarRepetidos :: [Int] -> [Int]
eliminarRepetidos []                              = []
eliminarRepetidos (x:xs) | primeroSeRepite (x:xs) = eliminarRepetidos xs
eliminarRepetidos (x:xs) | otherwise              = x:eliminarRepetidos xs

--Ejercicio 9

--maximo: nos devuelve el elemento maximo de la lista

maximo :: [Int] -> Int
maximo [n]                             = n
maximo (x:xs) | primeroEsMaximo (x:xs) = maximo (x:(quitar (head xs) xs))
maximo (x:xs) | otherwise              = maximo xs

--maximoPrimero: nos dice si el primer elemento de la lista es el maximo de la lista

primeroEsMaximo :: [Int] -> Bool
primeroEsMaximo [n]                   = True
primeroEsMaximo (x:xs) | x >= head xs = primeroEsMaximo (x:(quitar (head xs) xs))
primeroEsMaximo (x:xs) | otherwise    = False

--Ejercicio 10

--ordenar: dada una lista xs, nos devuelve otra lista con los elementos de xs ordenados de forma creciente

ordenar :: [Int] -> [Int]
ordenar xs = reverso (ordenarCreciente xs) 

--ordenarCreciente: dada una lista xs, nos devuelve otra lista con los elementos de xs ordenados de forma decreciente

ordenarCreciente :: [Int] -> [Int]
ordenarCreciente [n] = [n]
ordenarCreciente xs  = maximo xs:ordenarCreciente (quitar (maximo xs) xs)


--Ejercicio 11

--reverso: dada una lista, nos devuelve una lista con el orden invertido

reverso :: [Int] -> [Int]
reverso [n] = [n]
reverso xs  = (devuelveUltimo xs):reverso (quitar (devuelveUltimo xs) xs)