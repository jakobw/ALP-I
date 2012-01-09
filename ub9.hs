-- 61)
data WBUCH a b = Leer | Einf a b (WBUCH a b)
  deriving Show

finde :: Eq a => WBUCH a b -> a -> Maybe b
finde Leer x = Nothing
finde (Einf s t w) x
  | s == x = Just t
  | otherwise = finde w x

entf :: Eq a => WBUCH a b -> a -> WBUCH a b
entf Leer _ = Leer
entf (Einf s t w) x
 | s == x = w
 | otherwise = Einf s t $ entf w x

leer:: WBUCH a b
leer = Leer

-- 62)
zipDictionaries Leer Leer = Leer
zipDictionaries d Leer = d
zipDictionaries Leer d = d
zipDictionaries (Einf s1 t1 w1) (Einf s2 t2 w2) = if s1 > s2 then
                                                    Einf s1 t1 $ Einf s2 t2 rest
                                                  else
                                                    Einf s2 t2 $ Einf s1 t1 rest
  where
    rest = zipDictionaries w1 w2

-- 63)
data (Ord a) => Suchbaum a b = Leer' | Knoten a b (Suchbaum a b) (Suchbaum a b)

height Leer' = -1
height (Knoten a b l r) = 1 + max (height l) (height r)

-- 64)

data Baum a = BLeer | BKnoten a (Baum a) (Baum a) deriving Show

depthTree = depth 0
  where
    depth _ BLeer = BLeer
    depth i (BKnoten _ l r) = BKnoten i (depth (i+1) l) (depth (i+1) r)
