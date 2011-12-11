{-
  43)
  Schreiben Sie eine Funktion, die für einer Liste [a1, . . . , an] die gröÿte Summe ai + ai+1 + · · · + aj einer in ihr enthaltenen zusammenhängenden Teilfolge bestimmt. (Die Summe der leeren Folge ist 0.)
-}

biggestSeq :: (Num a, Ord a) => [a] -> a
biggestSeq []     = 0
biggestSeq (x:xs) = max (biggest (x:xs)) $ biggestSeq xs
  where
    biggest seq = maximum $ scanl1 (+) seq

{-
  47)
  (a) (5 Punkte) Schreiben Sie eine eigene show-Funktion für den Datentyp Uhrzeit aus der Vorlesung vom 16. 11. und deklarieren Sie ihn als Beispiel der Typklasse Show. Sie können dabei das angelsächsische Format wie in Aufgabe 8 nehmen oder das bei uns gebräuchliche 24-Stunden-Format.
  (b) (10 Punkte) Deklarieren Sie Uhrzeit als Beispiel der Typklasse Enum. Sie müssen dazu die Funktionen fromEnum und toEnum de􏰃nieren. Drücken Sie dann mit Hilfe der folgenden Funktion, die in der Typklasse Enum bereits vorde􏰃niert ist, die Liste der Uhrzeiten 􏰂alle 15 Minuten von 10:28 bis 16:13 Uhr􏰁 aus.
        enumFromThenTo x y z = map toEnum
                              [ fromEnum x, fromEnum y .. fromEnum z ]
-}

data Uhrzeit = Zeit {
    h::Int,
    m::Int
  }

-- a)
instance Show Uhrzeit where
  show (Zeit h m) = show h ++ ":" ++ show m

-- b)
instance Enum Uhrzeit where
  fromEnum (Zeit h m) = 60 * h + m
  toEnum i = Zeit (i `div` 60) (i `mod` 60)

-- > enumFromThenTo (Zeit 10 28) (Zeit 10 43) (Zeit 16 13)
-- => [10:28,10:43,10:58,11:13,11:28,11:43,11:58,12:13,12:28,12:43,12:58,13:13,13:28,13:43,13:58,14:13,14:28,14:43,14:58,15:13,15:28,15:43,15:58,16:13]
-- sweet!
