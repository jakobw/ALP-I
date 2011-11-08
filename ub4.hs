{-
  19)
  Drücken Sie die Funktion length mit Hilfe von sum und map und geeigneten selbstdefinierten Funktionen aus. (Eine Zeile sollte für die Definition reichen.)
-}

length' :: [a] -> Int
length' a = sum (map (\_ -> 1) a)
