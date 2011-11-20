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
