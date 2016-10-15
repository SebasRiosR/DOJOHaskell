x :: Int
x = 5

lucky :: (Integral a) => a -> String
lucky 7 = "Hoy es tu dia de suerte"
lucky x = "Hoy no es tu dia de suerte"

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x-1)

sumatoria :: Integer -> Integer
sumatoria 0 = 0
sumatoria x = x + sumatoria (x-1)

sumEvertyTwo :: [Integer] -> [Integer]
sumEvertyTwo [] = []
sumEvertyTwo (x:[]) = [x]
sumEvertyTwo (x:y:zs) = (x+y):sumEvertyTwo zs

intListLength :: [Int] -> Int
intListLength [] = 0
intListLength (x:za) = 1 + intListLength za

pesoTest :: Int -> [Char]
pesoTest peso
	|peso <= 40 = "Estas muy flaco"
	|peso <= 60 = "Estas bien de peso"
    |peso<=80 ="Estas gordo"
    |otherwise ="Estas super gordo :("

mayor :: Int-> Int -> Int
mayor a b =	
	if a > b 
		then a 
		else b

duplicarPares xs = [x*2| x <- xs, (mod x 2) == 0]

duplicarTriples xs = [x| x <- xs, (mod x 3) == 0]