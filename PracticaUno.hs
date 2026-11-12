-- Practica Uno

data Natural = Cero | S Natural deriving Show

a_natural :: Int -> Natural
a_natural 0= Cero
a_natural n= S( a_natural(n-1))

a_entero :: Natural->Int
a_entero Cero=0
a_entero (S n)= 1+ a_entero n

suma_nat :: Natural -> Natural -> Natural
suma_nat Cero b = b
suma_nat (S a) b = S (suma_nat a b)

mult_nat :: Natural -> Natural -> Natural
mult_nat n Cero = Cero
mult_nat b (S n)= suma_nat b (mult_nat b n)  

fibo :: Int -> Int
fibo 0=0
fibo 1=1
fibo n = fibo (n-1) + fibo (n-2)

mult :: Int -> Int -> Int
mult x 0 = 0
mult x y = x + mult x (y-1)

potencia :: Int -> Int -> Int
potencia n 0 = 1
potencia n m = n * potencia n (m-1)

facto :: Int -> Int 
facto 0=1
facto n = n * facto (n-1)

division :: Int -> Int -> Int
division s i
    | s<i=0
    | otherwise = 1 + division (s-i) i

maximo :: [Int] -> Int
maximo []= error "Esta vacio"
maximo [x]= x
maximo (x:xs) = max x (maximo xs)

data ArbolB a = Vacio | Nodo a (ArbolB a) (ArbolB a) deriving (Eq, Show)
nodos :: ArbolB a -> Int
nodos Vacio=0
nodos (Nodo _ izq der) = 1 + nodos izq + nodos der

profundidad :: ArbolB a -> Int
profundidad Vacio = 0
profundidad (Nodo _ izq der) = 1 + max (profundidad izq) (profundidad der)