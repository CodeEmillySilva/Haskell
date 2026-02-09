--1 - Ejercicio/Exercise/Exercício
ehTriangulo x y z = z < x+y && y < z + x && x < y + z

--2 - Ejercicio/Exercise/Exercício
tipoTriangulo x y z = if x == y && x == z then "equilatero" else (if x == y && x /= z && y /= z then "isoceles" else "escaleno")

--3 - Ejercicio/Exercise/Exercício
triangulo x y z = if z < x+y && y < z + x && x < y + z then (if x == y && x == z then "equilatero" else if x == y && x /= z && y /= z then "isoceles" else "escaleno") else "Nao e triangulo"

--4 - Ejercicio/Exercise/Exercício
somaPares 0 = 0
somaPares n = if rem n 2 == 0 then n + somaPares (n-1) else somaPares (n-1)

--5 - Ejercicio/Exercise/Exercício
somaPot2m m 0 = m
somaPot2m m n = (2 ^ n) * m + somaPot2m m (n-1)

--6 - Ejercicio/Exercise/Exercício
primo n = primo' n (n-1)
primo' n 1 = True
primo' n d = if rem n d == 0 then False else primo' n (d-1)

--7 - Ejercicio/Exercise/Exercício
seriePi n = seriePi' n 1 1
seriePi' n d s = if d<= n then s*4/d + seriePi' (n-1) (d+2) (negate s) else 0
