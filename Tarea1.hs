-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Victor Hernandez
--  Carnet: 20-10349
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--  Declaro que use de manera didactica la IA Gemini para entender mejor el enunciado de la tarea, 
--  para verificar la correccion de algunos casos de prueba y aprender bien la sintaxis del lenguaje.  
--  No declare codigo generado por la IA como propio.
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
esPalindromo :: String -> Bool
esPalindromo cadena = limpia == miReversa limpia
  where
    limpia = limpiar cadena

    -- Funcion auxiliar que permite dejar solo letras en minusculas
    limpiar [] = []
    limpiar (c:resto)
      | isAlphaNum c = toLower c : limpiar resto
      | otherwise    = limpiar resto

    -- Funcion auxiliar para invertir la cadena
    miReversa [] = []
    miReversa (c:resto) = miReversa resto ++ [c]

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez: La funcion implementada toma el primer elemento, en este caso x,
-- le pide a la recursion que le traiga de un solo golpe el mayor y el menor de todo
-- el resto de la lista(xs), para luego simplemente comparar x con esos dos valores al mismo tiempo
--
mayorYMenor :: Ord a => [a] -> Maybe (a, a)

mayorYMenor [] = Nothing
mayorYMenor [x] = Just (x,x)
mayorYMenor (x:xs) = 
  let Just (may,men) = mayorYMenor xs
  in Just (max x may, min x men)

-- A3
leerHora :: String -> Either String (Int, Int)
leerHora [a, b, ':', c, d]
  | isDigit a && isDigit b && isDigit c && isDigit d =
      let h = (digitToInt a * 10) + digitToInt b 
          m = (digitToInt c * 10) + digitToInt d
      in if h > 23 then Left "hora fuera de rango"
         else if m > 59  then Left "minutos fuera de rango"
         else Right (h,m)
leerHora _ = Left "formato inválido"

duracion :: String -> String -> Either String Int
duracion t1 t2  = case leerHora t1 of
  Left err -> Left err
  Right (h1, m1) -> case leerHora t2 of
    Left err -> Left err
    Right (h2, m2) -> 
      let total1 = (h1 * 60) + m1
          total2 = (h2 * 60) + m2
          resta = total2 - total1
      in if resta < 0
        then Right (resta + 1440)
        else Right resta

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral umbral xs =
  let (totalSuma, totalCantidad) = foldr (\x (suma, cantidad) -> if x > umbral then (suma + x, cantidad + 1) else (suma, cantidad)) (0,0) xs
  in if totalCantidad == 0 then Nothing
     else Just (totalSuma / totalCantidad)

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo (f, c) =
  [(f + df, c + dc) | (df, dc) <- [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)]
                    , f + df >= 1 && f + df <= 8
                    , c + dc >= 1 && c + dc <= 8]

enDosSaltos :: (Int, Int) -> Int
enDosSaltos inicial = length (foldr (\x acc -> if elem x acc then acc else x : acc) [] todosLosSaltos)
  where todosLosSaltos = [ c2 | c1 <- movimientosCaballo inicial, c2 <- movimientosCaballo c1 ]

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap f xs = foldr (\x acc -> f x : acc) [] xs

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter p xs = foldr (\x acc -> if p x then x : acc else acc) [] xs

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl f z xs = foldr (\x g -> (\v -> g (f v x))) (\v -> v) xs z
