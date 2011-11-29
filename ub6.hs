{-
  33)
  Die Funktion filter p l wählt aus einer Liste l alle Elemente aus, die das Prädikat (die Boolesche Funktion) p erfüllen. Definieren Sie eine solche Funktion
             filter:: (a->Bool) -> [a] -> [a]
  (a) rekursiv,
  (b) durch Listendurchlauf (Zermelo-Fränkel-Notation).
  (c) Schreiben Sie mit Hilfe von filter eine Funktion, die alle Leerzeichen aus einer Zeichenkette entfernt.
-}

filter', filter'' :: (a -> Bool) -> [a] -> [a]

-- a)
filter' _ [] = []
filter' p (x:xs) = if p x then x:rest else rest
  where rest = filter' p xs

-- b)
filter'' p l = [x | x <- l, p x]

-- c)
deleteSpaces :: String -> String
deleteSpaces = filter (/= ' ')

{-
  38)
  Schreiben Sie eine Funktion mult zur Multiplikation zweier Zahlen, die das zweite Argument nicht auswertet, wenn das erste Argument 0 ist.
-}

mult :: Num a => a -> a -> a
mult 0 _ = 0
mult a b = a * b
