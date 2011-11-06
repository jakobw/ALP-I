{-
  12)
  Geben Sie fuÌˆr jeden eingefuÌˆhrten Namen den GuÌˆltigkeitsbereich an.
  (a) x = 25:: Integer
  biggerThanAVG3:: Integer->Integer->Integer->Integer biggerThanAVG3 x y z =
              sum (map (\x -> if fromIntegral x > avg3 x y z then 1 else 0)
                       [x ,y, z])
              where avg3:: Integer->Integer->Integer->Double
                    avg3 a b c = fromIntegral (a+b+c) / 3
           test1:: Integer
           test1 = biggerThanAVG3 3 4 5
           flaÌˆche:: Float -> Float
           flaÌˆche x = 2*x^2*pi -- FlaÌˆche eines Kreises mit Radius x
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
  geben an, was gekauft werden soll und vieviel (in einer passenden Einheit, z. B. kg), zum Beispiel [("Mehl",0.5), ("Butter",0.25)], und andererseits den Preis in Euro pro Einheit fuÌˆr jeden Artikel.
  (a) Deí¯¿í°ƒnieren Sie eine Funktion
  preis:: Preisliste -> Einkaufsliste -> (Float,[String])
  zur Berechnung des Gesamtpreises aller Artikel einer Einkaufsliste. Die zweite Kom- ponente des Ergebnisses soll die Liste der Artikelnamen enthalten, die in der Preis- liste nicht gefunden wurden.
  (b) Erweitern Sie die Funktion so, dass der Preis fuÌˆr jeden gekauften Artikel der Ein- kaufsliste auf Cent gerundet wird.
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
  (b) Geben Sie eine alternative Deí¯¿í°ƒnition als Funktion iter n f ohne den Parameter x.
  (c) LoÌˆsen Sie Aufgabe 4b (Zinseszinsen) mit Hilfe der Funktion iter und der LoÌˆsung von Aufgabe 4a. (Achten Sie auf die Reihenfolge der Argumente.)
-}

{-
  16)
  Die Funktion logBase a x = loga x berechnet den Logarithmus von x zur Basis a; das
  istdieZahly,fuÌˆrdieay =xist.
  (a) Bestimmen Sie die kleinste Zahl x, fuÌˆr die logBase 2 x >= 5 ist.
  (b) Bestimmen Sie die kleinste Zahl x, fuÌˆr die iter 3 (logBase 2) x >= 2 ist.
-}

{-
  17)
  Das Potenzieren mit einer natuÌˆrlich Zahl als Exponent kann man als iteriertes Multi- plizierendeí¯¿í°ƒnieren:xn :=xÂ·xÂ·xÂ·Â·Â·xmitnFaktoren.
     potenz x n = iter n (x*) 1
  (a) Deí¯¿í°ƒnieren Sie unter Verwendung von potenz und iter die í¯¿í°Turmfunktioní¯¿í°‚ x â†‘ k:
  xxÂ·Â·Â·x
  wobei in dem Turm auf der rechten Seite x insgesamt k-mal vorkommt. (Potenzie-
  ren ist rechtsassoziativ!)
  (b) Die Multiplikation kann als iterierte Addition aufgefasst werden. Schreiben Sie eine entsprechende Funktion mal a b, die analog zur Funktion potenz das Produkt als iterierte Summe deí¯¿í°ƒniert.
  (c) Welche Funktion muss man iterieren, damit man die Summenfunktion plus a b = a + b erhaÌˆlt? Schreiben Sie eine entsprechende Funktionsdeí¯¿í°ƒnition fuÌˆr plus.
-}

{-
  (a) Jemand hat die Funktion g = iter 23 deí¯¿í°ƒniert. Wie koÌˆnnen Sie das Argument 23 aus der Funktion g herausí¯¿í°ƒnden? Schreiben Sie eine Funktion entdecke, die fuÌˆr alle n â‰¥ 0 die folgende Beziehung erfuÌˆllt:
  entdecke (iter n) == n,
  (b) Addition: Schreiben Sie eine Funktion sumiter mit der Eigenschaft
  sumiter (iter a) (iter b) == iter (a+b), fuÌˆr alle a, b â‰¥ 0.
  (c) Multiplikation: Schreiben Sie eine analoge Funktion proditer fuÌˆr das Produkt von a und b.
  turm x k=xâ†‘k:=x
  ,
-}