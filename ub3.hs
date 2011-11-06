{-
  12)
  Geben Sie für jeden eingeführten Namen den Gültigkeitsbereich an.
  (a) x = 25:: Integer
  biggerThanAVG3:: Integer->Integer->Integer->Integer biggerThanAVG3 x y z =
              sum (map (\x -> if fromIntegral x > avg3 x y z then 1 else 0)
                       [x ,y, z])
              where avg3:: Integer->Integer->Integer->Double
                    avg3 a b c = fromIntegral (a+b+c) / 3
           test1:: Integer
           test1 = biggerThanAVG3 3 4 5
           fläche:: Float -> Float
           fläche x = 2*x^2*pi -- Fläche eines Kreises mit Radius x
  (b) f x y =
  let n = 3 in take n (g y) ++ take n (g x) where g x = take n xys
                       where
                         xys = [x] ++ yxs
                         yxs = [y] ++ xys
  n = 10
-}

{-
  13)
  Schreiben Sie eine Funktion, die berechnet, wie viele Zahlen in einer Liste von Integer-Werten kleiner als der Durchschnittswert sind.
-}

smallerThanAvg :: [Integer] -> Int
smallerThanAvg n
  | n == [] = error "empty list"
  | otherwise = length [ x | x <- n, realToFrac x < average n ]
  where average n = realToFrac (sum n) / realToFrac (length n)

{-
  14)
  Listen der beiden folgenden Datentypen
       type Einkaufsliste = [(String, Float)]
       type Preisliste    = [(String, Float)]
  geben an, was gekauft werden soll und vieviel (in einer passenden Einheit, z. B. kg), zum Beispiel [("Mehl",0.5), ("Butter",0.25)], und andererseits den Preis in Euro pro Einheit für jeden Artikel.
  (a) De��nieren Sie eine Funktion
  preis:: Preisliste -> Einkaufsliste -> (Float,[String])
  zur Berechnung des Gesamtpreises aller Artikel einer Einkaufsliste. Die zweite Kom- ponente des Ergebnisses soll die Liste der Artikelnamen enthalten, die in der Preis- liste nicht gefunden wurden.
  (b) Erweitern Sie die Funktion so, dass der Preis für jeden gekauften Artikel der Ein- kaufsliste auf Cent gerundet wird.
-}

type Einkaufsliste = [(String, Float)]
type Preisliste    = [(String, Float)]

preis :: Preisliste -> Einkaufsliste -> (Float, [String])
preis pl el = (calculatePrice pl, notFound pl el)
  where
    calculatePrice []     = 0
    calculatePrice (x:xs) = snd x + calculatePrice xs
    notFound pl el        = map fst (filter (\x -> not (elem (fst x) (map fst pl))) el)

{-
  15)
  Die folgende Funktion iter wendet eine Funktion f n-mal hintereinander auf ein Ar- gument x an. Zum Beispiel liefert iter 3 f x das Ergebnis f(f(f(x))), das man in der Mathematik manchmal als f3(x) oder noch genauer f(3)(x) schreibt, damit man es nicht mit der Potenz (f(x))3 verwechselt.
  iter n f x |n==0 =x
        | n>0   = f (iter (n-1) f x)
  (a) Welchen Typ hat iter?
  (b) Geben Sie eine alternative De��nition als Funktion iter n f ohne den Parameter x.
  (c) Lösen Sie Aufgabe 4b (Zinseszinsen) mit Hilfe der Funktion iter und der Lösung von Aufgabe 4a. (Achten Sie auf die Reihenfolge der Argumente.)
-}

{-
  16)
  Die Funktion logBase a x = loga x berechnet den Logarithmus von x zur Basis a; das
  istdieZahly,fürdieay =xist.
  (a) Bestimmen Sie die kleinste Zahl x, für die logBase 2 x >= 5 ist.
  (b) Bestimmen Sie die kleinste Zahl x, für die iter 3 (logBase 2) x >= 2 ist.
-}

{-
  17)
  Das Potenzieren mit einer natürlich Zahl als Exponent kann man als iteriertes Multi- plizierende��nieren:xn :=x·x·x···xmitnFaktoren.
     potenz x n = iter n (x*) 1
  (a) De��nieren Sie unter Verwendung von potenz und iter die ��Turmfunktion�� x ↑ k:
  xx···x
  wobei in dem Turm auf der rechten Seite x insgesamt k-mal vorkommt. (Potenzie-
  ren ist rechtsassoziativ!)
  (b) Die Multiplikation kann als iterierte Addition aufgefasst werden. Schreiben Sie eine entsprechende Funktion mal a b, die analog zur Funktion potenz das Produkt als iterierte Summe de��niert.
  (c) Welche Funktion muss man iterieren, damit man die Summenfunktion plus a b = a + b erhält? Schreiben Sie eine entsprechende Funktionsde��nition für plus.
-}

{-
  (a) Jemand hat die Funktion g = iter 23 de��niert. Wie können Sie das Argument 23 aus der Funktion g heraus��nden? Schreiben Sie eine Funktion entdecke, die für alle n ≥ 0 die folgende Beziehung erfüllt:
  entdecke (iter n) == n,
  (b) Addition: Schreiben Sie eine Funktion sumiter mit der Eigenschaft
  sumiter (iter a) (iter b) == iter (a+b), für alle a, b ≥ 0.
  (c) Multiplikation: Schreiben Sie eine analoge Funktion proditer für das Produkt von a und b.
  turm x k=x↑k:=x
  ,
-}