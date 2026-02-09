--1 - Exercício/Exercise/Ejercicio
pertence :: Eq a => a -> [a] -> Bool
pertence e [] = False
pertence e (x:xs) = if e==x then True else pertence e xs

--2 - Exercício/Exercise/Ejercicio
intercessao [] ys = []
intercessao (x:xs) ys = if pertence x ys then x:intercessao xs ys
                        else intercessao xs ys

--3 - Exercício/Exercise/Ejercicio
inverso [] = []
inverso (x:xs) = inverso xs ++ [x]

--4 - Exercício/Exercise/Ejercicio
nPrimeiros _ [] = []
nPrimeiros 0 _ = []
nPrimeiros n (x:xs) = x:nPrimeiros (n-1) xs

nUltimos n xs = inverso(nPrimeiros n (inverso xs))

--5 - Exercício/Exercise/Ejercicio
soma2 [] _ = []
soma2 _ [] = []
soma2 (x:xs) (y:ys) = (x+y) : soma2 xs ys

--6 - Exercício/Exercise/Ejercicio
pot2 0 = []
pot2 n = pot2 (n-1) ++ [2^n]

--7 - Exercício/Exercise/Ejercicio
intercalacao [] ys = ys
intercalacao xs [] = xs
intercalacao (x:xs)(y:ys) = if (x<y) then x : intercalacao xs (y:ys)
                            else y : intercalacao (x:xs) ys

--8 - Exercício/Exercise/Ejercicio
menor [x] = x
menor (x:xs) = if (x < menor xs) then x
               else menor xs

--9 - Exercício/Exercise/Ejercicio
removerElem n [] = []
removerElem n (x:xs) = if (n==x) then xs
                       else x : removerElem n xs

--10 - Exercício/Exercise/Ejercicio
ordenar [] = []
ordenar xs = m : ordenar (removerElem m xs)
                where m = menor xs

--11 - Exercício/Exercise/Ejercicio
ins n [] = [n]
ins n (x:xs) = if (pertence n xs) then (x:xs)
               else if (n<x) then ordenar (n:(x:xs))
                    else if (n>x) then ordenar (x:(n:xs))
                         else x : ins n xs
                         
--12 - Exercício/Exercise/Ejercicio
enesimo 1 (x:xs) = x
enesimo n (x:xs) = enesimo (n-1) xs

--13 - Exercício/Exercise/Ejercicio
repetir 0 _ = []
repetir n e = e : repetir (n-1) e

--14 - Exercício/Exercise/Ejercicio
numString 0 = []
numString n = numString (div n 10) ++ [int2Char (rem n 10)]

int2Char :: Int -> Char
int2Char d = toEnum (d+48)

--15 - Exercício/Exercise/Ejercicio
stringNum [] = 0
stringNum (x:xs) = char2Int x * 10^(length (x:xs) - 1) + stringNum xs

char2Int :: Char -> Int
char2Int c = fromEnum c - fromEnum '0'

--16 - Exercício/Exercise/Ejercicio
bin2int [] = 0
bin2int (x:xs) = char2Int x * 2^(length(x:xs)-1) + bin2int xs

--17 - Exercício/Exercise/Ejercicio
int2bin n = inverso (int2binario n)
int2binario 0 = []
int2binario n = int2Char (rem n 2) : int2binario (div n 2)

--18 - Exercício/Exercise/Ejercicio
minusculas [] = []
minusculas (x:xs) = if (x<'a' || x>'z') then (toEnum ((fromEnum x)+32)) : minusculas xs
                    else x : minusculas xs
