estanRelacionados :: Float -> Float -> Bool
estanRelacionados x y = (x <= 3 && y <= 3) || (x > 3 && x <= 7 && 3 < y && y <= 7) || (x > 7 && y > 7 )

prodInt :: (Float,Float) -> (Float,Float) -> Float
prodInt (a,b) (c,d) = a*c + b*d

todoMenor :: (Float,Float) -> (Float,Float) -> Bool
todoMenor (a,b) (c,d) = a < c && b < d

distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos (a,b) (c,d) = sqrt((c-a)**2 + (d-b)**2)

sumaTerna :: (Int,Int,Int) -> Int
sumaTerna (a,b,c) = a + b + c

posicPrimerPar :: (Int,Int,Int) -> Int
posicPrimerPar (a,b,c) | mod a 2 == 0 = 1
posicPrimerPar (a,b,c) | mod b 2 == 0 = 2
posicPrimerPar (a,b,c) | mod c 2 == 0 = 3
posicPrimerPar (a,b,c) | mod (a + b + c) 2 /= 0 = 4

crearPar :: a -> b -> (a,b)
crearPar a b = (a,b)

invertir :: (a,b) -> (b,a)
invertir (a,b) = (b,a)