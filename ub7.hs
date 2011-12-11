{-
  43)
  Schreiben Sie eine Funktion, die für einer Liste [a1, . . . , an] die gröÿte Summe ai + ai+1 + · · · + aj einer in ihr enthaltenen zusammenhängenden Teilfolge bestimmt. (Die Summe der leeren Folge ist 0.)
-}

biggestSeq :: (Num a, Ord a) => [a] -> a
biggestSeq []     = 0
biggestSeq (x:xs) = max (biggest (x:xs)) $ biggestSeq xs
  where
    biggest seq = maximum $ scanl1 (+) seq
