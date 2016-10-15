toDigitsRev :: Int -> [Int]
toDigitsRev num =
	if num <= 0
		then []
		else (mod num 10):toDigitsRev (div num 10)

toDigits :: Int -> [Int]
toDigits a =
	reverse (toDigitsRev a)

intListLength :: [Int] -> Int
intListLength [] = 0
intListLength (x:za) = 1 + intListLength za

--Entrada: Lista de numeros
--Salida: La lista con los indices 
--          pares multiplicados por 2
--          empezando por el final
--Ejemplo: [1,2,3,4,5]->[1,4,3,8,5]

doubleEveryOther :: [Int]->[Int]
doubleEveryOther [] = []
doubleEveryOther n
    |(intListLength n `mod` 2) == 0 = pair n
    |otherwise = nonpair n

pair :: [Int]->[Int]
pair []         = []
pair (x:xs:r)   = x * 2 : xs : doubleEveryOther r

nonpair :: [Int]->[Int]
nonpair (x:[])      = x:[]
nonpair (x:xs:r)    = x: xs * 2 : doubleEveryOther r


--Ejercicio 3
sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits (x:y) =
	if (x `mod` 10) == x
		then x + sumDigits(y)
		else (div x 10) + (x `mod` 10) + sumDigits(y)


--Ejercicio 4
punt :: Int -> Bool
punt a = sumDigits(doubleEveryOther(toDigits a)) `mod` 10 == 0

	








