{-
  19)
  Drücken Sie die Funktion length mit Hilfe von sum und map und geeigneten selbstdefinierten Funktionen aus. (Eine Zeile sollte für die Definition reichen.)
-}

length' :: [a] -> Int
length' a = sum (map (\_ -> 1) a)

{-
  20)
  Definieren Sie die Funktionen
        takeWhile :: (a -> Bool) -> [a] -> [a]
        splitAt :: Int -> [a] -> ([a],[a])
  Der Ausdruck takeWhile p x erzeugt das Anfangsstück einer Liste x, solange die Elemente das Prädikat p erfüllen. splitAt n x spaltet die Liste x nach den ersten n Elementen in zwei Teile auf. (Diese Funktionen sind in Haskell schon definiert. Zum Testen müssen Sie daher andere Funktionennamen wählen.)
-}

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x = x:takeWhile' p xs
  | otherwise = []

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n x  =  (take n x, drop n x)

{-
  21)
  Welchen Typ hat der Ausdruck map (`zinsen` 2.25) im Zusammenhang von Aufgabe 4? Was bewirkt diese Funktion?
-}

zinsen kapital zinsfuß = kapital * zinsfuß * 0.01

blah :: [Double] -> [Double]
blah = map (`zinsen` 2.25)
-- Die Funktion berechnet die Zinsen zu einer Liste aus Kapitalangaben mit dem vorgegebenen Zinsfuß 2.25. (2.25 gibt den Wert für zinsfuß vor, weil zinsen als Infix-Funktion geschrieben ist.)
