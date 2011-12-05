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
  36)
  (a) Schreiben Sie eine Funktion, die für eine Darstellung vom Typ Ausdruck (Vorle- sung vom 16. 11. 2011), die keine Variablen enthält, den durch den arithmetischen Ausdruck gegebenen Wert berechnet.
  (b) Schreiben Sie eine Funktion, die als zusätzliche Eingabe eine Werteliste vom Typ [(String,Integer)] akzeptiert, wobei ein Element ("x",3) zum Beispiel bedeu- ten soll, dass die Variable mit Namen "x" den Wert 3 hat. Falls der Ausdruck eine Variable enthält, die nicht in der Liste vorkommt, soll die Funktion mit einer informativen Fehlermeldung abbrechen.
-}
data Ausdruck =
    Konst Integer
   |Var String
   |Plus Ausdruck Ausdruck
   |Mal Ausdruck Ausdruck
  deriving Show

-- a)
eval :: Ausdruck -> Integer
eval (Konst i)  = i
eval (Plus a b) = eval a + eval b
eval (Mal a b)  = eval a * eval b

-- b)
eval' :: Ausdruck -> [(String, Integer)] -> Integer
eval' (Konst i) _  = i
eval' (Var x) vars = getValue $ filter (\ (i, _) -> x == i) vars
  where
    getValue []         = error "Couldn't find variable."
    getValue ((_, i):_) = i
eval' (Plus a b) vars = eval' a vars + eval' b vars
eval' (Mal a b) vars  = eval' a vars * eval' b vars

{-
  38)
  Schreiben Sie eine Funktion mult zur Multiplikation zweier Zahlen, die das zweite Argument nicht auswertet, wenn das erste Argument 0 ist.
-}

mult :: Num a => a -> a -> a
mult 0 _ = 0
mult a b = a * b

{-
  40)
  (a) Die Folge F1, F2, . . . der Fibonacci-Zahlen ist durch die Anfangswerte F1 = F2 = 1 und die Rekursion
      Fn = Fn−1 + Fn−2 (∗) für n ≥ 3 gegeben. Schreiben Sie ein Programm zur Berechnung der n-ten Fibonacci-
      Zahl. Berechnen Sie die ersten 20 Fibonacci-Zahlen.
  (b) Welche Werte ergeben sich für Fn mit n ≤ 0, wenn man die Gültigkeit der Gleichung (∗) auf alle ganzen Zahlen n ausdehnt? Erweitern Sie Ihr Programm, sodass es auch eine negative Eingabe akzeptiert.
-}

-- a + b)
fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n
  | n > 0 = fib (n-2) + fib (n-1)
  | otherwise = fib (n+2) - fib (n+1)

-- [fib x | x <- [1..20]] => [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]
