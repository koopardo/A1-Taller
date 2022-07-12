--Ejercicio 1

absoluto :: Int -> Int
absoluto n | n >= 0 = n
           | n < 0 = -n

--Ejercicio 2

maximoabsoluto :: Int -> Int -> Int
maximoabsoluto n m | absoluto n >= absoluto m = absoluto n
                   | otherwise = absoluto m

--Ejercicio 3

maximo3 :: Int -> Int -> Int -> Int
maximo3 a b c = maximoabsoluto (maximoabsoluto a b) (maximoabsoluto b c)

--Ejercicio 4

algunoEsCero :: Float -> Float -> Bool
algunoEsCero a b | a == 0 || b == 0 = True
                 | otherwise = False

--Usando pattern matching

algunoEsCero2 :: Float -> Float -> Bool
algunoEsCero2 0 b = True
algunoEsCero2 a 0 = True
algunoEsCero2 _ _ = False     

--Ejercicio 5

ambosSonCero :: Float -> Float -> Bool
ambosSonCero a b | a == 0 && b == 0 = True
                 | otherwise = False

--Usando pattern matching

ambosSonCero2 :: Float -> Float -> Bool
ambosSonCero2 0 0 = True
ambosSonCero2 _ _ = False                        

--Ejercicio 6

esMultiploDe :: Int -> Int -> Bool
esMultiploDe n m | mod n m == 0 = True
                 | otherwise = False  

--Ejercicio 7

digitoUnidades :: Int -> Int
digitoUnidades n = mod n 10   

--Ejercicio 8

digitoDecenas :: Int -> Int
digitoDecenas n = mod (div n 10) 10

digitoDecenas2 :: Int -> Int
digitoDecenas2 n = div (mod n 100) 10