{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}
xor a b = (a && not b) || (not a && b) 
impl a b = not a || b 
equiv a b = impl a b && impl b a

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}
square x = x*x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow x y = x ** y


{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}
fatorial 0 = 1 
fatorial x = x * fatorial (x - 1)

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}

isPrime 1 = True
isPrime 2 = True
isPrime 3 = True
isPrime x = checkPrime x [2 .. (x - 1)]

checkPrime x []  = True
checkPrime x (y:ys) = x `mod` y /= 0 && checkPrime x ys

{-  
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}

fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-}
mdc x y 
    | x `mod` y == 0 = y
    | otherwise = mdc y (x `mod` y)

{-
- Calcula um MMC de dois numeros. 
-}
mmc x y =  x * y `div` mdc x y

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}
coprimo x y = mdc x y == 1

{-
- Calcula a conjectura de Goldbach, que diz que um numero par maior que 2 pode ser escrito como a soma de dois numeros primos. Ex: 28 = 5 + 23.
    A função filter recebe um argumento (isPrime return == True) e o conjunto de valores para filtrar quais deles satisfazem 
-}
goldbach x = [(y, z) |  y <- filter isPrime [1 .. (x - 1)], z <- filter isPrime [1..(x - 1)],  y + z == x]
