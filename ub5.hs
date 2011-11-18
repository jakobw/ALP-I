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
