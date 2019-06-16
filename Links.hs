-- Dieses Modul stellt alle wesentlichen Funktionen zur Erstellung und Manipulation
-- von Verschlingungsdiagrammen, sowie Funktionen zur Bestimmung der Orientierung.

module Links (
    Crossing (..),
    Link,
    right_handed,
    n_plus,
    n_minus,
    mirror,
    disjoint_union,
    knot_sum
) where

-- Definition von Überkreuzungen und Verschlingungen
data Crossing = X[Int] deriving (Show)
type Link = [Crossing]


-- Bestimmung der Orientierung einer Verschlingung
right_handed :: Crossing -> Bool
right_handed (X[i,j,k,l]) = j-l == 1 || l-j > 1

n_plus :: Link -> Int
n_plus link = length [c | c <- link, right_handed c]

n_minus :: Link -> Int
n_minus link = length link - n_plus link


-- Eine Verschlingung spiegeln
mirrorc :: Crossing -> Crossing
mirrorc (X[i,j,k,l]) = 
    if j-l == 1 || l-j > 1 then
        X[l,i,j,k]
    else
        X[j,k,l,i]

mirror :: Link -> Link
mirror link = map mirrorc link


-- Die disjunkte Vereinigung zweier Diagramme bilden
shift_numbers :: Int -> Link -> Link
shift_numbers n [] = []
shift_numbers n ((X[i,j,k,l]):ls) = 
    (X[i+n,j+n,k+n,l+n]):(shift_numbers n ls)

disjoint_union :: Link -> Link -> Link
disjoint_union link_a link_b = 
    link_a ++ ((shift_numbers n) link_b)
    where n = length link_a * 2


-- In einer Liste Zahlen ersetzen
replace :: (Eq a) => a -> a -> [a] -> [a]
replace x y [] = []
replace x y (a:as) =
    if a == x then
        y:(replace x y as)
    else
        a:(replace x y as)


-- Passt die Kante mit einem Sprung in der Nummerierung an
connect :: Int -> Crossing -> Crossing
connect c (X[i,j,k,l]) =
    if abs (i-k) > 1 || abs (j-l) > 1 then
        X (replace 1 c [i,j,k,l])
    else
        X[i,j,k,l]


-- Die Knotensumme zweier Diagramme bilden
knot_sum :: Link -> Link -> Link
knot_sum link_a link_b = 
    disjoint_union 
        (map (connect (n+1)) link_a) 
        (map (connect (1-m)) link_b)
    where (n,m) = (length link_b * 2, length link_a * 2)