{-
  25)
  Korrigieren Sie die Benennung der Variablen in der Funktion biggerThanAVG3 von Aufgabe 12a (3. Übungsblatt) so, dass die Funktion berechnet, wie viele der drei Eingabeparameter größer als der Durchschnittswert sind (Aufgabe 6 vom 1. Übungsblatt).

  biggerThanAVG3:: Integer->Integer->Integer->Integer
  biggerThanAVG3 x y z =
            sum (map (\x -> if fromIntegral x > avg3 x y z then 1 else 0)
                     [x ,y, z])
            where avg3:: Integer->Integer->Integer->Double
                  avg3 a b c = fromIntegral (a+b+c) / 3
-}
biggerThanAVG3 :: Integer->Integer->Integer->Integer
biggerThanAVG3 a b c = sum (map (\x -> if fromIntegral x > avg3 a b c then 1 else 0) [a, b, c])
  where
    avg3 :: Integer->Integer->Integer->Double
    avg3 a b c = fromIntegral (a+b+c) / 3
{-
  26)
  Wandeln Sie folgende Funktionsdefinitionen in anonyme Funktionen um, ohne lokale Definitionen mit where oder let zu verwenden. Ihre Lösung soll mit f = \. . . beginnen.
    (a) Die Funktion f = biggerThanAVG3 von Aufgabe 12a (3. Übungsblatt)
    (b) Die Funktion f von Aufgabe 12b (3. Übungsblatt)
    (c) Ihre Lösung von Aufgabe 25.
    (d) f x y z = x^3 - g (x + g (y - g z) + g (z^2)) where g x = 2*x^2 + 10*x + 1
    Ein Trick: Sie können sich das explizite Hineinkopieren der Funktion g an mehrere Stellen sparen, indem Sie zunächst g als zusätzlichen (ersten) Parameter Ihrer Funktion defnieren.
-}

-- a)
fA :: Integer -> Integer -> Integer -> Integer
fA = \ x y z -> sum (map (\x -> if fromIntegral x > fromIntegral (x+y+z) / 3 then 1 else 0) [x, y, z])

-- b)
fB :: a -> a -> [a]
fB x y = (\ n g -> take n (g y) ++ take n (g x)) 3 (\ x -> [x, y, x])

-- c) Entweder wie in (a) mit anderen Variablennamen oder die, wie ich finde, etwas lesbarere Variante:
fC :: Integer -> Integer -> Integer -> Integer
fC = \ a b c -> toEnum $ length $ filter (\ x -> fromInteger x > (fromInteger $ a + b + c) / 3) [a, b, c]

-- d)
fD :: Integer -> Integer -> Integer -> Integer
fD = (\ g x y z -> x^3 - g (x + g (y - g z) + g (z^2))) (\ x -> 2*x^2 + 10*x + 1)

{-
  27)
  Die Funktionen scanr und scanl liefern eine Liste mit allen Zwischenergebnissen der Funktion foldr beziehungsweise foldl:
       scanr:: (a -> b -> b) -> b -> [a] -> [b]
       scanl:: (b -> a -> b) -> b -> [a] -> [b]
       scanr op z []     = [z]
       scanr op z (x:xs) = (x `op` q) : q q:qs
                             where (q:qs) = scanr op z xs
       scanl op a []     = [a]
       scanl op a (x:xs) = a : scanl op (a `op` x) xs
  Eine Funktion soll berechnen, wie sich ein Anfangskapital k0 über mehrere Jahre entwickelt, wenn die Folge [z1, z2, . . . , zn] der jährlichen Verzinsungen gegeben ist. Verwenden Sie dazu die Funktion scanr oder scanl, sowie geeignete Funktionen von Aufgabe 4.
-}

zinsen kapital zinsfuß = kapital * zinsfuß * 0.01
capitalGrowth c = scanl (\ c i -> c + zinsen c i) c

{-
  29)
  Bei diesem Sortierverfahren wird das kleinste Element einer Liste ausgewählt und an den Anfang gestellt; die restlichen Elemente werden rekursiv sortiert. Programmieren Sie dieses Sortierverfahren in Haskell.
-}

sort' :: (Ord a) => [a] -> [a]
sort' [] = []
sort' x = minimum x : sort' (rest x)
  where
    minX = minimum x
    rest (x:xs) = if x == minX then xs else x:rest xs

{-
  30)
  (a) Definieren Sie die Funktion ++ durch eine geeignete Faltung.
  (b) Bei der Lösung von Aufgabe 9 war eine Funktion hilfreich, die eine Liste von Zeichenketten zu einer einzigen Zeichenkette zusammenfügt (In der Musterlösung heißt diese Funktion aneinander:: [String] -> String, in Haskell gibt es dafür die Standardfunktion concat:: [[a]] -> [a].) Definieren Sie diese Funktion durch Faltung.
-}

-- a)
plusplus :: [a] -> [a] -> [a]
plusplus x y = foldr (:) y $ foldr (:) x []

-- b)
concat' :: [[a]] -> [a]
concat' = foldr1 (++)

{-
  31)
  (a) Definieren Sie eine Funktion, die eine Liste von Listen mit einem Verbindungsglied zusammenfügt. Das Verbindungsglied soll zwischen den Elementen erscheinen, aber nicht am Ende nach dem letzten Element. Hier sind Beispiele:
        verbinden :: [a] -> [[a]] -> [a]
        verbinden ", " [] = ""
        verbinden ", " ["eins"] = "eins"
        verbinden ", " ["erstens","2.","drittens"] = "erstens, 2., drittens"
  (b) Die Umkehrfunktion trennen :: [a] -> [a] -> [[a]] soll die Liste an den Stel- len trennen, wo das Verbindungsglied vorkommt, sodass für alle x die Gleichung verbinden x . trennen x = id gilt. Überlegen Sie, ob die Lösung immer ein- deutig ist. Beschreiben Sie klar und eindeutig (d.h., spezifizieren Sie), was Ihre Umkehrfunktion in jedem Fall machen soll.
  (c) Definieren Sie die Umkehrfunktion nach Ihrer Spezifikation.
  (d) Warum ist es unmöglich, durch eine Funktion trennen für alle x die Gleichung trennen x . verbinden x = id zu erfüllen? Gibt es Werte von x, für die man die Beziehung erfüllen kann?
-}

-- a)
verbinden :: [a] -> [[a]] -> [a]
verbinden conn = foldr (\ x acc -> if null acc then x else x ++ conn ++ acc) []

-- b)
{-
  Die Funktion `trennen` trennt eine Liste [a] an einem Listenglied [b] auf und fügt die einzelnen Teile aus [a] zu einer Liste [[a]] zusammen.
-}

-- c)
-- TODO: eleganter?
trennen :: Eq a => [a] -> [a] -> [[a]]
trennen _ [] = []
trennen pattern x = el: trennen pattern (drop (length el + pLength) x)
  where
    el = getEl pattern x
    pLength = length pattern

    getEl _ [] = []
    getEl pattern (x:xs) = x: if take pLength xs == pattern && pLength /= length xs then [] else getEl pattern xs

-- d)
{-
  Eines der Elemente aus [[a]] könnte das Trennmuster enthalten und damit bewirken, dass beim Aufruf der Funktion `trennen` an der falschen Stelle getrennt wird. Ansonsten funktioniert auch `trennen x . verbinden x = id`.
-}

{-
  32)
  Welche der folgenden Funktionen ersetzt jedes Vorkommen eines bestimmten Wortes in einer Liste von Wörtern durch eine Folge von Sternen? Bestimmen Sie für jede Funktion, ob Sie überhaupt nach den Haskell-Regeln gültig ist, und stellen Sie gegebenenfalls ihren Typ fest.
-}

wiederhole n x = [ x | k <- [1 .. n]] -- = die Standardfunktion replicate
ersetzeWort unwort w
  | unwort == w = wiederhole (length unwort) '*'
  | otherwise = w

zensiere1, zensiere5, zensiere6 :: String -> [String] -> [String]
zensiere1 unwort = map (ersetzeWort unwort) -- funktioniert

zensiere2, zensiere8 :: [String] -> [String -> String]
zensiere2 unwort = map ersetzeWort unwort -- nimmt eine Liste von Strings an und gibt eine Liste von ersetzeWort Funktionen mit vorgegebenem Unwort-Parameter zurück... nicht das, was wir wollten.

-- zensiere3 unwort = map unwort . ersetzeWort

-- zensiere4 unwort = map . ersetzeWort unwort

zensiere5 = map . ersetzeWort -- funktioniert

zensiere6 unwort = (map . ersetzeWort) unwort -- funktioniert

-- zensiere7 unwort = ersetzeWort unwort . map

zensiere8 = map ersetzeWort -- == zensiere2
