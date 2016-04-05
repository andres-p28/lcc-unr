module Lab01 where

import Data.List

{-
1) Corregir los siguientes programas de modo que sean aceptados por GHCi.
-}

-- a)
not' b = case b of
    True  -> False
    False -> True

-- b)
deletelast [x]         = []
deletelast (x:xs)      =  x : deletelast xs
deletelast []          =  error "empty list"

-- c)
length' []        =  0
length' (_:l)     =  1 + length' l

-- d)
list123 = 1 : (2 : (3 : []))

-- e)
[]     ++! ys = ys
(x:xs) ++! ys = x : (xs ++! ys)

-- f)
addToTail x xs = map (+x) (tail xs)

-- g)

listmin xs = head (sort xs)

-- h) (*)
smap f []     = []
smap f [x]    = [f x]
smap f (x:xs) = f x : (smap f xs)

{-
    2. Definir las siguientes funciones y determinar su tipo:
  a) five, que dado cualquier valor, devuelve 5
-}

five :: a -> Int
five x = 5

{-
  b) apply, que toma una funciÃ³n y un valor, y devuelve el resultado de
  aplicar la funciÃ³n al valor dado
-}

apply :: (a -> b) -> a -> b
apply f x = f x

{-
  c) ident, la funciÃ³n identidad
-}

ident :: a -> a
ident x = x

{-
  d) first, que toma un par ordenado, y devuelve su primera componente
-}

first :: (a,b) -> a
first (a, _) = a

{-
  e) derive, que aproxima la derivada de una funciÃ³n dada en un punto dado
-}

derive :: (Float -> Float) -> Float -> Float
derive f x = (f (x + 0.0001) - f x) / 0.0001

{-
  f) sign, la funciÃ³n signo
-}

sign :: Float -> Float
sign x | x < 0  = -1
       | x == 0 = 0
       | x > 0  = 1
       
{- 
  g) vabs, la funciÃ³n valor absoluto (usando sign y sin usarla)
-}

vabs :: Float -> Float
vabs x = (sign x) * x

vabs' :: Float -> Float
vabs' x | x < 0  = -x
        | x >= 0 = x 

{-
h) pot, que toma un entero y un nÃºmero, y devuelve el resultado de
elevar el segundo a la potencia dada por el primero
-}

pot :: Int -> Int -> Int
pot y x = x^y

{- 
  i) xor, el operador de disyunciÃ³n exclusiva
-}

xor :: Bool -> Bool -> Bool
xor t p = t /= p

{-
  j) max3, que toma tres nÃºmeros enteros y devuelve el mÃ¡ximo entre llos
-}

max3 :: Int -> Int -> Int -> Int
max3 x y z = if x > y then 
                if x > z then x else z 
             else y

{-
  k) swap, que toma un par y devuelve el par con sus componentes invertidas
-}

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

{- 
  3) Definir una funciÃ³n que determine si un aÃ±o es bisiesto o no, de
  acuerdo a la siguiente definiciÃ³n:

aÃ±o bisiesto 1. m. El que tiene un dÃ­a mÃ¡s que el aÃ±o comÃºn, aÃ±adido al mes de febrero. Se repite
cada cuatro aÃ±os, a excepciÃ³n del Ãºltimo de cada siglo cuyo nÃºmero de centenas no sea mÃºltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22Âª ed.)

Â¿CuÃ¡l es el tipo de la funciÃ³n definida?
-}

bisiesto :: Int -> Bool
bisiesto a = (a `mod` 4 == 0 && a `mod` 100 /= 0) || a `mod` 400 == 0

{-
4)

Defina un operador infijo *$ que implemente la multiplicaciÃ³n de un
vector por un escalar. Representaremos a los vectores mediante listas
de Haskell. AsÃ­, dada una lista ns y un nÃºmero n, el valor ns *$ n
debe ser igual a la lista ns con todos sus elementos multiplicados por
n. Por ejemplo,

[ 2, 3 ] *$ 5 == [ 10 , 15 ].

El operador *$ debe definirse de manera que la siguiente
expresiÃ³n sea vÃ¡lida:

v = [1, 2, 3] *$ 2 *$ 4

-}

(*$) :: [Int] -> Int -> [Int]
(*$) ns n = map (*n) ns


{-
5) Definir las siguientes funciones usando listas por comprensiÃ³n:

a) 'divisors', que dado un entero positivo 'x' devuelve la
lista de los divisores de 'x' (o la lista vacÃ­a si el entero no es positivo)

b) 'matches', que dados un entero 'x' y una lista de enteros descarta
de la lista los elementos distintos a 'x'

c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
donde 0 <= a, b, c, d <= 'n'

(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos
-}

unique :: [Int] -> [Int]
unique xs = [x | (x,i) <- zip xs [0..], not (elem x (take i xs))]

{- 
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posiciÃ³n) de ambas listas.  Definir una funciÃ³n 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}

{-
7) Definir mediante recursiÃ³n explÃ­cita
las siguientes funciones y escribir su tipo mÃ¡s general:

a) 'suma', que suma todos los elementos de una lista de nÃºmeros

b) 'alguno', que devuelve True si algÃºn elemento de una
lista de valores booleanos es True, y False en caso
contrario

c) 'todos', que devuelve True si todos los elementos de
una lista de valores booleanos son True, y False en caso
contrario

d) 'codes', que dada una lista de caracteres, devuelve la
lista de sus ordinales

e) 'restos', que calcula la lista de los restos de la
divisiÃ³n de los elementos de una lista de nÃºmeros dada por otro
nÃºmero dado

f) 'cuadrados', que dada una lista de nÃºmeros, devuelva la
lista de sus cuadrados

g) 'longitudes', que dada una lista de listas, devuelve la
lista de sus longitudes

h) 'orden', que dada una lista de pares de nÃºmeros, devuelve
la lista de aquellos pares en los que la primera componente es
menor que el triple de la segunda

i) 'pares', que dada una lista de enteros, devuelve la lista
de los elementos pares

j) 'letras', que dada una lista de caracteres, devuelve la
lista de aquellos que son letras (minÃºsculas o mayÃºsculas)

k) 'masDe', que dada una lista de listas 'xss' y un
nÃºmero 'n', devuelve la lista de aquellas listas de 'xss'
con longitud mayor que 'n' -}
