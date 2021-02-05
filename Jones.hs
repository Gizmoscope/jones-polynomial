-- Dieses Modul stellt die Funktionen zur Berechnung des Jones-Polynom bereit
-- Im Grunde genommen sind es nur die Funktionen "kauffman_bracket" und
-- "unnormalized_jones_polynomial".

module Jones (
    kauffman_bracket,
    unnormalized_jones_polynomial
) where

import Links
import Poly

-- Das Polynom X + X^-1.
circle_poly :: Poly Int
circle_poly = Poly 1 [1,0,1]

-- Ersetzt in einer Liste jeden Eintrag x durch y.
replace :: (Eq a) => a -> a -> [a] -> [a]
replace x y [] = []
replace x y (a:as) =
    if a == x then
        y:(replace x y as)
    else
        a:(replace x y as)

-- Erzwingt die Gleichheit zweier Kanten in einer Überkreuzung, dazu nehmen beide
-- Kanten den Wert der kleineren Kante an.
equalize :: Int -> Int -> Crossing -> Crossing
equalize x y (X ls) = (X (replace (max x y) (min x y) ls))

-- Definition analog zur mathematischen Definition der Kauffman-Klammer
-- Das Auflösen der Überkreuzungen geschieht durch das "Verbinden" entsprechender
-- Kanten (siehe "kauffman_bracket_modulo").
kauffman_bracket :: Link -> Poly Int
kauffman_bracket [] = 1
kauffman_bracket (X[i,j,k,l]:link) = 
    (kauffman_bracket_modulo i j k l link) 
        - (shift_poly 1 (kauffman_bracket_modulo i l j k link))

-- Berechnet die Kauffman-Klammer einer Verschlingung bei der Kanten verbunden
-- werden. Anschaulich werden "i" mit "j" und "k" mit "l" verbunden. Die Kanten
-- werden verbunden indem die Nummern der Kanten gleich gesetzt werden (siehe
-- equalize). In einigen Sonderfällen können durch die Verbindungen Kreise
-- entstehen. Entspechend der Kauffman-Klammer werden diese Kreise durch
-- Multiplikation mit "circle_poly" entfernt.
kauffman_bracket_modulo :: Int -> Int -> Int -> Int -> Link -> Poly Int
kauffman_bracket_modulo i j k l link
  | i==j && k==l = circle_poly^2 * (kauffman_bracket link)
  | i==k && j==l = circle_poly   * (kauffman_bracket link)
  | i==l && k==j = circle_poly   * (kauffman_bracket link)
  | i==j         = circle_poly   * (kauffman_bracket (map (equalize k l) link))
  | k==l         = circle_poly   * (kauffman_bracket (map (equalize i j) link))
  | i==k || i==l || j==k || j==l = kauffman_bracket 
      (map (equalize (min i j) (min k l)) 
      (map (equalize i j) 
      (map (equalize k l) link)))
  | otherwise = kauffman_bracket (map (equalize i j) (map (equalize k l) link))

-- Berechnet das nichtnormierte Jones-Polynom über die Kauffman-Klammer und
-- "n_plus" und "n_minus" der Verschlingung.
unnormalized_jones_polynomial :: Link -> Poly Int
unnormalized_jones_polynomial link = 
    poly_smult ((-1)^(n_minus link)) (Poly (2*(n_minus link) 
        - (n_plus link)) [1]) * (kauffman_bracket link)
