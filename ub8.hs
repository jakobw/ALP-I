-- 48)
deleteTrailingWhitespace :: IO ()
deleteTrailingWhitespace = do
  putStr "File 1:"
  f1 <- getLine
  putStr "File 2:"
  f2 <- getLine
  content <- readFile f1
  writeFile f2 $ deleteSpaces $ map reverse $ lines content
    where
      deleteSpaces s = unlines $ map (reverse . dropWhile (== ' ')) s

-- 49)
data Baum' a = Blatt a | Knoten a (Baum' a) (Baum' a) deriving Show
testBaum = "27D5D-2UD7UUD44D8UD5D7UD9UUU"

decodeTree :: String -> Baum' Int
decodeTree t
  | elem 'D' tree = Knoten (read $ takeWhile (/= 'D') t :: Int) (decodeTree $ fst nodes) (decodeTree . drop 1 $ snd nodes)
  | otherwise = Blatt (read $ filter (/= 'U') tree :: Int)
    where
      tree = if elem 'D' t then drop 1 $ dropWhile (/= 'D') t else t

      nodes = foldl returnTree ("", "") tree
        where
          returnTree (n1, n2) c
            | numD n1 + 1 == numU n1 && numD n1 > 0 = (n1, n2 ++ [c])
            | numD n1 < numU n1                     = (n1, n2 ++ [c])
            | otherwise                             = (n1 ++ [c], n2)

          numD = length . filter (== 'D')
          numU = length . filter (== 'U')

-- 52)
data Ord a => Suchbaum a b = Leer | Knoten' a b (Suchbaum a b) (Suchbaum a b)
  deriving Show

finde :: Ord a => a -> Suchbaum a b -> Maybe b
finde _ Leer = Nothing
finde x (Knoten' a b t1 t2)
  | x == a    = Just b
  | x > a     = finde x t2
  | otherwise = finde x t1

einf :: Ord a => a -> b -> Suchbaum a b -> Suchbaum a b
einf i v (Knoten' iK iV l r)
  | i < iK = Knoten' iK iV (einf i v l) r
  | i > iK = Knoten' iK iV l $ einf i v r
einf i v Leer = Knoten' i v Leer Leer