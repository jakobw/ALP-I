{-
  7)
  Uhrzeiten wie 10:30 Uhr oder 23:59 Uhr sollen als geordnete Paare (10, 30) beziehungs- weise (23,59) dargestellt werden.
  Schreiben Sie eine Funktion, die die Dauer zwischen zwei Uhrzeiten in Stunden und Minuten berechnet (negativ, falls die zweite Uhrzeit vor der ersten liegt).
  Sie sollten das Problem in Teilprobleme zerlegen und geeignete Hilfsfunktionen definieren.
-}

timeDiff :: (Int, Int) -> (Int, Int) -> (Int, Int)
timeDiff a b = minsToTime (timeToMins b - timeToMins a)
  where minsToTime mins = (mins `div` 60, mins `mod` 60)
        timeToMins (hours, mins) = hours * 60 + mins

{-
  8)
  Im angelsa ̈chsischen Raum werden Uhrzeiten in einem 12-Stunden-Bereich angegeben und mit dem Zusatz a.m. (Vormittag), p.m. (Nachmittag), noon oder midnight verse- hen: 0:00 = 12:00 midnight, 0:13 = 12:13 a.m., 10:30 = 10:30 a.m., 12:00 = 12:00 noon, 12:55 = 12:55 p.m., 13:20 = 1:20 p.m., 23:59 = 11:59 p.m.. Die Stundenzahl ist also immer zwischen 1 und 12.
  Schreiben Sie eine Funktion, die dieses Format (als Zeichenkette) berechnet, wenn die Eingabe wie in Aufgabe 7 gegeben ist. Wenn die Eingabe keine gu ̈ltige Uhrzeit darstellt, soll der Text ungu ̈ltig ausgegeben werden.
-}

timeToString :: (Int, Int) -> String
timeToString (hours, mins)
  | hours > 24 || hours < 0 || mins > 59 || mins < 0 = "ungültig"
  | otherwise = show (fst (hourFormat hours)) ++ ':':show mins ++ ' ':snd (hourFormat hours)
  where hourFormat hours
          | hours > 12 && hours < 24 = (hours - 12, "p.m.")
          | hours > 0 && hours < 12 = (hours, "a.m.")
          | hours == 12 = (12, "noon")
          | hours == 0 = (12, "midnight")
          | otherwise = error "ungültig"

{-
  9)
  Erstellen Sie eine Tabelle fu ̈r das kleine Einmaleins (die Produkte
  aller Zahlenpaare zwischen 1 und 10) als Zeichenkette. Die einzel-
  nen Zeilen sind mit ’\n’ abgeschlossen. Das reduzierte Beispiel
  rechts zeigt, wie so eine Tabelle aussehen ko ̈nnte, wenn man sie
  mit putStr ausgibt. Die Spalten ihrer Tabelle mu ̈ssen vertikal ausgerichtet sein. (Hinweis: die Funktion show wandelt eine Zahl in eine Zeichenkette um.)
-}

timesTable :: (Int, Int) -> String
timesTable (rows, cols) = "\t| " ++ showCols cols ++ "\n" ++ tab ++ "+-" ++ underline cols ++ "\n" ++ showRows rows cols
  where tab = "--------"
        showCols 0 = ""
        showCols n = showCols (n-1) ++ show n ++ "\t"

        underline 1 = ""
        underline n = tab ++ underline (n - 1)

        showRows 0 _ = ""
        showRows row cols = showRows (row - 1) cols ++ show row ++ "\t| " ++ multiply row cols ++ "\n"
          where multiply _ 0 = ""
                multiply row col = multiply row (col - 1) ++ show (row * col) ++ "\t"

{-
  *Main> putStr (timesTable (10, 10))
          | 1     2       3       4       5       6       7       8       9       10
  --------+-------------------------------------------------------------------------
  1       | 1     2       3       4       5       6       7       8       9       10
  2       | 2     4       6       8       10      12      14      16      18      20
  3       | 3     6       9       12      15      18      21      24      27      30
  4       | 4     8       12      16      20      24      28      32      36      40
  5       | 5     10      15      20      25      30      35      40      45      50
  6       | 6     12      18      24      30      36      42      48      54      60
  7       | 7     14      21      28      35      42      49      56      63      70
  8       | 8     16      24      32      40      48      56      64      72      80
  9       | 9     18      27      36      45      54      63      72      81      90
  10      | 10    20      30      40      50      60      70      80      90      100
-}

{-
  10)
  Schreiben Sie die folgende Funktion als einen einzigen (nicht geschachtelten) if-then- else-Ausdruck:
       test x y z
         | x <= y    = True
         | y <= z    = False
         | otherwise = x < z
  Zusatzfrage (0 Punkte): Kommt man auch ganz ohne if-then-else-Ausdruck aus?
-}

test x y z = if x <= y then True else False

-- Zusatz:
test' x y z = x <= y

{-
  11)
  (a) Schreiben Sie eine Funktion, die zu einer Zahl n die Liste ihrer (positiven) Teiler berechnet.
  (b) Bestimmen Sie jede Zahl n zwischen 1 und 1000, bei der die Summe ihrer von n verschiedenen Teiler gro ̈ßer ist als die Zahl n selbst.
  (Hinweis: Die Funktion sum berechnet die Summe der Elemente einer Liste)
  (c) Bestimmen Sie alle perfekten Zahlen zwischen 1 und 1000: die Zahlen n, die gleich der Summe ihrer von n verschiedenen Teiler sind.
-}
-- a)
divisors n = [ x | x <- [1..n-1], n `mod` x == 0 ]

--b)
divisorsBiggerThanX = [x | x <- [1..1000], sum (divisors x) > x]

-- c)
divisorsEqualX = [x | x <- [1..1000], sum (divisors x) == x]
