{-
  19)
  Drücken Sie die Funktion length mit Hilfe von sum und map und geeigneten selbstdefinierten Funktionen aus. (Eine Zeile sollte für die Definition reichen.)
-}

length', length'' :: [a] -> Int
length' a = sum (map (\_ -> 1) a)

length'' = sum.map (const 1)

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

{-
  24)
  (a) (10 Punkte) Bei der Lauflängenkodierung wird eine Kette "aaaabbaaa" mit vielen wiederholten Zeichen komprimiert, indem man die Länge jedes Laufes von gleichen Zeichen nimmt: [(4,'a'), (2,'b'), (3,'a')]. Schreiben Sie eine Funktion, die diese Kodierung berechnet, und auch die Umkehrfunktion für die Dekodierung.
  (b) (10 Punkte) Unter der Annahme, dass die Eingabekette keine Ziffern enthält, kann man die komprimierte Fassung kompakter als Kette "4a2b3a" darstellen. Erweitern Sie die vorige Aufgabe auf diese Darstellung
-}

-- a)
runLengthDecode :: [(Int, Char)] -> String
runLengthDecode list = foldr (\(n, chr) text -> replicate n chr ++ text) "" list

runLengthEncode :: String -> [(Int, Char)]
runLengthEncode "" = []
runLengthEncode str =
  let count = countFirst str
  in (count, head str): runLengthEncode (drop count str)
  where
    countFirst str = length (takeWhile (== head str) str)

-- b)
runLengthDecode' :: String -> String
runLengthDecode' [] = ""
runLengthDecode' (n:chr:xs) = replicate (read [n]) chr ++ runLengthDecode' xs

runLengthEncode' :: String -> String
runLengthEncode' str = foldr (\(n, chr) encStr -> show n ++ [chr] ++ encStr) "" (runLengthEncode str)
