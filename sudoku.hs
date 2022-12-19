import System.Random
import Data.Map
--PRÁCTICA HASKELL
--Arturo Precioso Garcelán

--RESUMEN GENERAL
--sudoku.hs es capaz de resolver sudokus, generar sudokus y puedes jugar con sudokus de cualquier tamaño (a,b), donde (a,b) es el tamaño de las cuadriculas del sudoku (lo que está separado por rayas).
--solucionar_iter tiene en cuenta las posibles soluciones de cada casilla y resuelve el sudoku iterativamente para hayar solucón. sudoku_jugable crea un sudoku para jugar.
--Para hacer uso de estas funciones usamos iniciar_sudoku para crear un archivo txt para almacenar tu sudoku y luego abrimos este archivos con jugar archivo para iniciar el juego. La  tupla para elegir la casilla
--es (de izqda a dcha, de arriba abajo) empenzando por el (1,1) 
-- DEFINICIÓN DE DATOS:
data Sudoku = Sudoku [[Celda]] (Int,Int)
   deriving(Eq, Read, Show)

data Celda = Def Int | May [Int]
   deriving(Eq, Read)
   
instance Show Celda where
   show (Def x) = show x
   show (May xs) = show 0

type Pos = (Int, Int)
--FUNCIONES ÚTILES GENERICAS
--remover remueve los elementos de la segunda lista que estén en la primera
remover::[Int] -> [Int] -> [Int]
remover [] ys = ys
remover xs [] = []
remover (x:xs) ys = remover xs (Prelude.filter (/=x) ys)

--remover_n remueve la n-ésima posición de una lista
remover_n :: Int -> [a] -> [a]
remover_n _ [] = []
remover_n 0 (x:xs) = xs
remover_n n (x:xs) = x:remover_n (n-1) xs

--linea_rayas crea una linea de n - 
linea_rayas:: Int -> [String]
linea_rayas n = replicate n "-"

--tail_n enseña los n últimos elementos de una lista
tail_n:: Int -> [a] -> [a]
tail_n n xs = reverse (take n (reverse xs))

--init_n remueve los n últimos elementos de una lista
init_n:: Int -> [a] -> [a]
init_n 0 xs = xs
init_n n xs = init_n (n-1) (init xs)

--comunes devuelve una lista de los elementos comunes de dos listas
comunes:: Eq a => [a] -> [a] -> [a]
comunes _ [] = []
comunes [] _ = []
comunes (x:xs) ys
   |elem x ys = x:(comunes xs ys)
   |otherwise = comunes xs ys

--nocomunes devuelve una lista de los elementos no comunes de dos listas
nocomunes:: Eq a => [a] -> [a] -> [a]
nocomunes _ [] = []
nocomunes [] _ = []
nocomunes (x:xs) ys
   |elem x ys = comunes xs ys
   |otherwise = x:(comunes xs ys)
   
--esta_en_lista dice si algo está en una lista
esta_en_lista:: Eq a => [a] -> a -> Bool
esta_en_lista xs x = foldr (||) False (Prelude.map (==x) xs)

--transponer transpone una matriz
transponer :: [[a]] -> [[a]]
transponer [] = []
transponer ([]:xss) = transponer xss
transponer ((x:xs):xss) = (x : fmap principio xss) : transponer (xs : fmap final xss)
    where principio (x:_) = x
          final (_:xs) = xs

--FUNCIONES ÚTILES PARA SUDOKUS
  
--columna y fila devuelven la columna o fila en la n-ésima posición  
columna:: Sudoku -> Int -> [Celda]
columna (Sudoku xs (a,b)) x =  Prelude.map (!!x) xs

fila:: Sudoku -> Int -> [Celda]
fila (Sudoku xs _) x = xs !! x

--transponer_sudoku transpone la matriz del sudoku
transponer_sudoku:: Sudoku -> Sudoku
transponer_sudoku (Sudoku xss (a,b)) = Sudoku (transponer xss) (a,b)

--subcuadricula indica la subcuadricula (x,y)
subcuadricula:: Sudoku -> Pos -> [[Celda]]
subcuadricula a p  = subcuadricula2 (subcuadricula1 a p) p

subcuadricula1:: Sudoku -> Pos -> Sudoku
subcuadricula1 (Sudoku xs (u,v)) (a,b)
	|a == 0 = Sudoku (Prelude.map (take u) xs) (u,v)
	|otherwise = subcuadricula1 (Sudoku (Prelude.map (drop u) xs) (u,v)) (a-1,b)
	
subcuadricula2:: Sudoku -> Pos -> [[Celda]]
subcuadricula2 (Sudoku xs (u,v)) (a,b)
   |b == 0 = take v xs
   |otherwise = subcuadricula2 (Sudoku (drop v xs) (u,v)) (a,b-1)
--int_a_celda pasa de un Int a Celda (si es 0 es un May)
int_a_celda:: (Int,Int) -> Int -> Celda
int_a_celda (a,b) 0 = crear_may (a,b)
int_a_celda _ x = Def x

--fila_ordenada genera [1 .. n] pero en tipo Celda 
fila_ordenada:: Int -> [Celda] 
fila_ordenada n = Prelude.map (int_a_celda (1,1)) [1 .. n]

--cambiar_celda cambia una celda en la posicion que quieras por la celda que le des
cambiar_celda:: Sudoku -> Pos -> Celda -> Sudoku
cambiar_celda (Sudoku xss (a,b)) (u,v) x = Sudoku (yss++[cambiar_celda_aux (Sudoku xss (a,b)) (u,v) x]++(drop 1 zss)) (a,b)
   where (yss,zss) = splitAt v xss

cambiar_celda_aux:: Sudoku -> Pos -> Celda -> [Celda]
cambiar_celda_aux (Sudoku xss (a,b)) (u,v) x = xs++[x]++(drop 1 ys)
   where (xs,ys)= splitAt u (fila (Sudoku xss (a,b)) v)

--celda te devuelve la celda de un sudoku en la posición que le des
celda:: Sudoku -> Pos -> Celda
celda (Sudoku xss (a,b)) (u,v) = (xss!!v)!!u 

--pos_a_sub indica el subcuadrado en el que se encuentra la posición que le des
pos_a_sub:: Pos -> Pos -> Pos
pos_a_sub (x,y) (a,b) = (x `div` a, y `div` b)

--generar_rectangulo nos genera rectangulos que cumplen las reglas del sudoku
generar_rectangulo:: [Celda] -> (Int,Int) -> [[Celda]]
generar_rectangulo _ (a,0) = []
generar_rectangulo xs (a,b) = xs:(generar_rectangulo (ts++hs) (a,b-1))
   where (hs,ts) = splitAt a xs
 
--sudoku_ordenado nos da un sudoku (a,b) resuelto ordenado 
sudoku_ordenado:: (Int,Int) -> [[Celda]]
sudoku_ordenado (a,b) = generar_sudoku_ordenado (a,b) 0 []

generar_sudoku_ordenado:: (Int,Int) ->  Int ->  [[Celda]] -> [[Celda]]
generar_sudoku_ordenado (a,b) 0 _ = generar_sudoku_ordenado (a,b) 1 (generar_rectangulo(fila_ordenada (a*b)) (a,b)) 
generar_sudoku_ordenado (a,b) n xss 
   |n == a = xss
   |otherwise = generar_sudoku_ordenado (a,b) (n+1)   (xss++ (generar_rectangulo (ts++hs) (a,b)))
   where (hs,ts) = splitAt n (fila_ordenada (a*b))   


--ALEATORIEDAD Y GENERAR NUESTRO SUDOKU PARA JUGAR

--fisherYates baraja una lista, nos evita instalar System.Random.Shuffle
fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate = zip [1..]
    initial x gen = (singleton 0 x, gen)

--generar_sudoku nos genera un sudoku resuelto aleatorio un numero predeterminado de veces
generar_sudoku:: StdGen -> (Int,Int) -> Sudoku
generar_sudoku rng (a,b) = barajar_bloques rng (barajar_sudoku rng (barajar_digitos rng (Sudoku xss (a,b)) 20) 100) 40
   where xss = sudoku_ordenado (a,b)

--barajar_sudoku baraja las filas y columnas de el sudoku
barajar_sudoku:: StdGen -> Sudoku -> Int -> Sudoku
barajar_sudoku rng (Sudoku xss (a,b)) n
   |n == 0 = Sudoku xss (a,b)
   |n >= 50 = barajar_sudoku (snd $ next rng)  (Sudoku yss (a,b)) (n-1)
   |otherwise = barajar_sudoku (snd $ next rng) (transponer_sudoku (Sudoku zss (a,b))) (n-1)
   where yss = barajar_filas rng (Sudoku xss (a,b))
         zss = barajar_columnas rng (Sudoku tss (a,b))
         tss = transponer xss

--barajar_filas/columnas baraja las filas/columnas del sudoku
barajar_filas:: StdGen -> Sudoku -> [[Celda]]
barajar_filas rng (Sudoku [] (a,b)) = []
barajar_filas rng (Sudoku xss (a,b)) = zss++ barajar_filas (snd $ next rng) (Sudoku (drop b xss) (a,b))
   where (zss,_) = fisherYates rng (take b xss)

barajar_columnas:: StdGen -> Sudoku -> [[Celda]]
barajar_columnas rng (Sudoku [] (a,b)) = []
barajar_columnas rng (Sudoku xss (a,b))  = zss++barajar_columnas (snd $ next rng) (Sudoku (drop a xss) (a,b))
   where (zss,_) = fisherYates rng (take a xss)

--barajar_digitos cambia los numeros (ej. todos los unos pasan a ser doses)
barajar_digitos:: StdGen -> Sudoku -> Int ->Sudoku
barajar_digitos rng s n
   |n == 0 = s
   |otherwise = barajar_digitos (snd $ next (snd $ next rng)) (permutar_aleatorio rng s) (n-1)

--permutar cambia un numero por otro y viceversa
permutar::Eq a => a -> a -> [a] ->[a]
permutar a b xs = Prelude.map (\x -> if x == a then b else if x == b then a else x) xs

--permutar_aleatorio escoge dos digitos aleatorios para aplicarles permutar
permutar_aleatorio:: StdGen -> Sudoku -> Sudoku
permutar_aleatorio rng (Sudoku xss (a,b)) = Sudoku (Prelude.map (permutar x y) xss) (a,b)
   where x = Def (fst $ randomR (1,a*b) rng)
         y = Def (fst $ randomR (1,a*b) (snd $ next rng))

--barajar_bloques baraja los bloques (lo que está separado por rayas) del sudoku
barajar_bloques:: StdGen -> Sudoku -> Int -> Sudoku
barajar_bloques rng (Sudoku xss (a,b)) n
   |n == 0 = Sudoku xss (a,b)
   |n >= 20 = barajar_bloques (snd $ next rng) (Sudoku yss (a,b)) (n-1)
   |otherwise = barajar_bloques (snd $ next rng) (transponer_sudoku (Sudoku zss (a,b))) (n-1)
   where yss = desbloques_aux(fst $ fisherYates rng (bloques_aux xss b []))
         zss = desbloques_aux(fst $ fisherYates rng (bloques_aux tss a []))
         tss = transponer xss

--bloques_aux separa la matriz sudoku en sus bloques horizontales
bloques_aux::[[Celda]] -> Int -> [[[Celda]]] -> [[[Celda]]]
bloques_aux [] n xsss = xsss
bloques_aux xss n xsss = bloques_aux (drop n xss) n ((take n xss):xsss) 

--desbloques_aux deshace bloques_aux
desbloques_aux:: [[[Celda]]] -> [[Celda]]
desbloques_aux xsss = foldr (++) [] xsss

--sudoku_jugable crea un sudoku de solución única con un número arbitrario de casillas quitadas
sudoku_jugable:: StdGen -> (Int,Int) -> Sudoku
sudoku_jugable _ (1,1) = Sudoku [[May [1]]] (1,1)
sudoku_jugable rng (a,b) = quitar_piezas rng (generar_sudoku rng (a,b)) 100

--quitar_piezas quita piezas del sudoku siempre que sea válido
quitar_piezas::StdGen -> Sudoku -> Int -> Sudoku
quitar_piezas rng (Sudoku xss (a,b))  n
   |n == 0 = (Sudoku xss (a,b))
   |es_def(celda (Sudoku xss (a,b)) (x,y)) == False = quitar_piezas (snd $ next (snd $ next rng)) (Sudoku xss (a,b))  n
   |esta_resuelto z == False =  quitar_piezas (snd $ next (snd $ next rng)) (Sudoku xss (a,b))  (n -1)
   |otherwise = quitar_piezas (snd $ next rng) s  n
   where x = (fst $ randomR (0,(a*b)-1) rng)
         y = (fst $ randomR (0,(a*b)-1) (snd $ next rng))
         s = cambiar_celda (Sudoku xss (a,b)) (x,y) (crear_may (a,b))
         z = solucionar_iter s

--SOLUCIONAR EL SUDOKU

--crear_may crea una celda May xs con xs todos los digitos posibles del sudoku
crear_may:: (Int,Int) -> Celda
crear_may (a,b) = May [1 .. (a*b)] 
 
--celdas_a_posible coge una lista de celdas y devuelve una lista de los números ocupados definitivos
celdas_a_posible:: [Celda] -> [Int] -> [Int]
celdas_a_posible [] ys = ys
celdas_a_posible ((May x):xs) ys = celdas_a_posible xs ys
celdas_a_posible ((Def x):xs) ys = celdas_a_posible xs (x:ys)

--es_def dice si una celda es del tipo Def x
es_def:: Celda -> Bool
es_def (Def x) = True
es_def _ = False

--unmay coge la lista de un May xs
unmay:: Celda -> [Int]
unmay (Def x) = [x]
unmay (May xs) = xs

--posible_num cambia una celda a todos los posibles números que esta pueda ser
posible_num:: Sudoku -> Pos -> Sudoku
posible_num s p = cambiar_celda s p (posible_num_aux s p )

posible_num_aux:: Sudoku -> Pos -> Celda
posible_num_aux (Sudoku xss (a,b)) p 
   |es_def (celda (Sudoku xss (a,b)) p ) = celda (Sudoku xss (a,b)) p 
   |length vs == 1 = Def (head vs)
   |otherwise = May vs
   where vs = comunes xs (comunes ys zs)
         xs = posible_cuadrado (Sudoku xss (a,b)) p 
         ys = posible_columna (Sudoku xss (a,b)) p 
         zs = posible_fila (Sudoku xss (a,b)) p

--posible_cuadrado/fila/columna mira que números no están definitivamente en ese cuadrado/fila/columna
posible_cuadrado:: Sudoku -> Pos -> [Int]
posible_cuadrado (Sudoku xss (a,b)) (u,v) = Prelude.filter (\x -> not (x `elem` (celdas_a_posible (foldr (++) [] (subcuadricula (Sudoku xss (a,b)) (pos_a_sub (u,v) (a,b)))) [])))   [1 .. (a*b)]

posible_columna:: Sudoku -> Pos -> [Int]
posible_columna (Sudoku xss (a,b)) (u,v) = Prelude.filter (\x -> not (x `elem` (celdas_a_posible (columna (Sudoku xss (a,b)) u) []))) [1 .. (a*b)]

posible_fila:: Sudoku -> Pos -> [Int]
posible_fila (Sudoku xss (a,b)) (u,v) = Prelude.filter (\x -> not (x `elem` (celdas_a_posible (fila (Sudoku xss (a,b)) v) []))) [1 .. (a*b)]

--solucionar_facil usa posible_num para resolver todas las casillas que pueda / reducir su lista May
solucionar_facil:: Sudoku -> Sudoku
solucionar_facil (Sudoku xss (a,b)) =  foldr (flip posible_num) (Sudoku xss (a,b)) [ (x,y) | x<-[0 .. ((a*b)-1)] , y<-[0 .. ((a*b)-1)] ]

--solucionar_facil_iter aplica la anterior función hasta que no pueda seguir
solucionar_facil_iter:: Sudoku -> Sudoku
solucionar_facil_iter s
   |s == t = t
   |otherwise = solucionar_facil t
   where t = solucionar_facil s 

--redumay_celda mira si una celda tiene un May único en su cuadrado/fila/columna (esto sirve si por ejemplo una casilla es la única que pueda alojar un 3 en su fila, entonces es un 3 definitivamente)
redumay_celda:: Celda -> [[Int]] -> Celda
redumay_celda (Def x) _ = Def x
redumay_celda (May xs) xss 
   |length ys == 1 = Def (head ys)
   |otherwise = May xs
   where ys = foldr (remover) xs xss

--redumay_fila/columna/cuadrado mira si la posición tiene algún May único en su fila/columna/cuadrado
redumay_fila:: Sudoku -> Pos -> Sudoku
redumay_fila s (u,v) = cambiar_celda s (u,v) (redumay_celda (celda s (u,v)) (remover_n u (Prelude.map (unmay) (fila s v))))

redumay_columna:: Sudoku -> Pos -> Sudoku
redumay_columna s (u,v) = cambiar_celda s (u,v) (redumay_celda (celda s (u,v)) (remover_n v (Prelude.map (unmay) (columna s u))))

redumay_cuadrado:: Sudoku -> Pos -> Sudoku
redumay_cuadrado (Sudoku xss (a,b)) (u,v) = cambiar_celda (Sudoku xss (a,b)) (u,v) (redumay_celda (celda (Sudoku xss (a,b)) (u,v)) (remover_n z (Prelude.map (unmay)(foldr (++) [](subcuadricula (Sudoku xss (a,b)) (x,y))))))
   where (x,y) = pos_a_sub (u,v) (a,b)
         z = b*(mod v b) + (mod u a) 

--redumay aplica todas las anteriores
redumay:: Sudoku -> Pos -> Sudoku
redumay s p 
   |es_def (celda s p) = s
   |s /= redumay_cuadrado s p = redumay_cuadrado s p
   |s /= redumay_fila s p = redumay_fila s p
   |s /= redumay_columna s p = redumay_columna s p
   |otherwise = s

--solucionar aplica tanto solucionar_facil como redumay a todo el sudoku
solucionar:: Sudoku -> Sudoku
solucionar (Sudoku xss (a,b)) =  foldr (flip redumay) (solucionar_facil (Sudoku xss (a,b))) [ (x,y) | x<-[0 .. ((a*b)-1)] , y<-[0 .. ((a*b)-1)] ]

--solucionar_iter aplica solucionar hasta que pare
solucionar_iter:: Sudoku -> Sudoku
solucionar_iter s
   |s == t = t
   |otherwise = solucionar t
   where t = solucionar s 

--esta_resuelto solo comprueba si todas las celdas del sudoku son Def
esta_resuelto:: Sudoku -> Bool
esta_resuelto (Sudoku xss (a,b)) = foldr (&&) True (Prelude.map (foldr (&&) True) (Prelude.map (Prelude.map (es_def)) xss)) 

--ENSEÑAR EL SUDOKU EN PANTALLA

--sudoku_bonito enseña en pantalla el sudoku en su rejilla
sudoku_bonito:: Sudoku -> String
sudoku_bonito (Sudoku [[x]] (1,1)) = show_bonito x
sudoku_bonito (Sudoku xss (a,b)) = unlines $ Prelude.map (unwords) (anadir_separadores (a,b) (Prelude.map (fila_bonita a) xss))

--anadir_separadores añade las rejillas al sudoku
anadir_separadores:: (Int,Int) -> [[String]] -> [[String]]
anadir_separadores (a,b) xss = init (anadir_separadores_aux (a,b) xss)

anadir_separadores_aux:: (Int,Int) -> [[String]] -> [[String]] 
anadir_separadores_aux _ [] = [] 
anadir_separadores_aux (a,b) xss = yss++[(linea_rayas ((a*b)+b-1))]++(anadir_separadores_aux (a,b) zss)
   where (yss,zss) = splitAt b xss

--fila_bonita pasa una lista de celdas a los números que vemos con los separadores de columnas
fila_bonita:: Int -> [Celda] -> [String]
fila_bonita n xs = init (fila_bonita_aux xs n)

fila_bonita_aux:: [Celda] -> Int -> [String]
fila_bonita_aux [] _ = []
fila_bonita_aux xs n = (Prelude.map show_bonito ys)++["|"]++(fila_bonita_aux zs n)
   where (ys,zs) = splitAt n xs

--show_bonito enseña un entero si la celda es Def y enseña 0 si es May
show_bonito:: Celda -> String
show_bonito (Def x) = show x
show_bonito (May xs) = show 0

--JUGAR A Sudoku
--iniciar_sudoku crea un fichero que almacena un sudoku del tamaño que pidas
iniciar_sudoku:: IO()
iniciar_sudoku = do rng <- newStdGen
                    putStr "Dame el nombre del nombre del sudoku: "
                    nombre <- getLine
                    putStr "Tamano del sudoku (formato (x,y)): "
                    tamano <- getLine
                    let sudoku = sudoku_jugable rng (read tamano)
                    putStrLn (sudoku_bonito sudoku)
                    putStrLn "jugar nombre para jugar"
                    writeFile nombre (drop 7 (show sudoku))

--a_sudoku pasa el contenido del fichero a un tipo Sudoku
a_sudoku:: [Char] -> (Int,Int)  -> Sudoku
a_sudoku xss (a,b)  = Sudoku (Prelude.map (Prelude.map (int_a_celda (a,b))) (read  xss)) (a,b) 

--corregir facilita poner las celdas bien a la hora de jugar (en mi opinión es más claro que la esquina de arriba a la izquiera sea la posición (1,1) y no (0,0))
corregir:: Pos -> Pos
corregir (a,b) = (a-1,b-1)

--jugar te guía a la hora de jugar al sudoku. Eliges la posición que quieres cambiar (el primer número de la tupla recorre de izqda a dcha, el segundo de arriba abajo)
--No te deja poner una solución incorrecta, puedes elegir que te de una pista de qué puede ir ahí o no. Desde ese menú también se puede acceder secretamente a que te dé
--la solución de esa casilla o la solución de todo el sudoku. Si en cualquier momento frenamos la ejecución del bucle podemos volver a abrir nuestro fichero sudoku y seguir jugándolo.
jugar:: String-> IO()
jugar nombre = do sudoku <- readFile nombre 
                  let tamano = tail_n 5 sudoku
                  let s = a_sudoku (init_n 6 sudoku) (read tamano)
                  if esta_resuelto s
                     then do putStrLn "Sudoku resuelto, enhorabuena!"
                     else do putStrLn (sudoku_bonito s)
                             putStr "Posicion a cambiar (x,y): "
                             posicion <- getLine
                             putStr "Por que numero lo quieres cambiar: "
                             numero <- getLine
                             if es_def (celda s (corregir (read posicion)))
                                then do putStrLn "Esa casilla ya era buena"
                                        jugar nombre 
                                else do let z = cambiar_celda s (corregir (read posicion)) (Def (read numero))
                                        let sol = solucionar_iter s
                                        if celda z (corregir (read posicion)) == celda sol (corregir (read posicion))
                                           then do writeFile nombre $ (drop 7 (show z))
                                                   jugar nombre
                                           else do putStr "Ese numero no encaja ahi, si quieres una pista escribe s, sino escribe n: "
                                                   pista <- getLine
                                                   if pista == "s"
                                                      then do let v = posible_num_aux s (corregir (read posicion))
                                                              if es_def v
                                                                 then do putStrLn ("La unica opcion posible es "++(show_bonito v))
                                                                         jugar nombre
                                                                 else do putStrLn ("La lista de opciones es "++(show(unmay v)))
                                                                         jugar nombre
                                                      else if pista == "megapista"
                                                              then do putStrLn "LA SUPERSECRETA MEGAPISTA ;)"
                                                                      putStrLn ("La casilla es un " ++ (show (celda sol (corregir (read posicion)))))
                                                                      jugar nombre
                                                      else if pista == "solucion"
                                                              then do putStrLn "La solucion es:"
                                                                      putStrLn (sudoku_bonito sol)
                                                      else do jugar nombre