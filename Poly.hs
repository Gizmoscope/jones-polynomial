-- Dieses Modul stellt den Typ "Poly a" bereit, welcher Laurent-Polynome darstellt.
-- Bei Laurent-Polynomen sind negative Exponenten erlaubt. Die Koeffizienten
-- werden in einer Liste gespeichert. Um die Koeffizienten zu den negativen
-- Exponenten zu repräsentieren, werden die Koeffizienten der Liste künstlich
-- verschoben. Die Koeffizienten mit kleinen Exponenten befinden sich am Anfang
-- der Liste.
--
-- Die Typ-Variable "a" bezeichnet den Typ der Koeffizienten. Die meisten
-- Funktionen auf den Polynomen erfordern, dass dieser Typ eine Ringstruktur
-- aufweist. In Haskell bedeutet dass, dass "a" zur Typklasse "Num" und "Eq"
-- gehört. Typischerweise hat man "a = Int". Die Polynome bilden wieder eine
-- Ringstruktur, was bedeutet, dass "Poly a" ebenfalls zur Typklasse "Num" und
-- "Eq" gehört.
--
-- Dieses Modul stellt nur die nötigsten Funktionalitäten zur Verfügung, genau
-- die, die für die Berechnung des Jones-Polynom benötigt werden. Es besteht
-- aber die Möglichkeit zur Erweiterung.

module Poly (
    Poly (..),
    shift_poly,
    poly_add,
    poly_mult,
    poly_smult
) where

import Data.List

-- Der Konstruktor für Polynome verlangt eine Liste von Koeffizienten und einen
-- Faktor, welcher die Verschiebung der Koeffizienten in Richtung der negativen
-- Potenzen beschreibt.
data Poly a = Poly { shift :: Int, coeff :: [a] }
    deriving (Read)

-- Polynome lassen sich vergleichen. Dazu werden die Polynome auf eine Normalform
-- (siehe "reduce") gebracht, wo anschließend nur noch Verschiebefaktor und 
-- Koeffizientenliste auf Gleichheit geprüft werden muss. 
instance (Eq a, Num a) => Eq (Poly a) where
    p == q = eq2 (reduce p) (reduce q)

-- Polynome weisen eine Ringstruktur auf. Addition und Multiplikation werden
-- weiter unten definiert. Die ganzen Zahlen werden als konstante Polynome
-- eingebettet. "abs" und "sgn" haben hier keine Semantik.
instance (Num a) => Num (Poly a) where
    (+) = poly_add
    (*) = poly_mult
    fromInteger n = Poly 0 [fromInteger n]
    negate = poly_smult (-1)
    abs = id
    signum p = 1

-- Um Polynome in lesbarer Form anzuzeigen, werden sie zunächst auf Normalform
-- gebracht. "show'" erzeugt die Ausgabe (weiter unten).
instance (Eq a, Num a, Show a) => Show (Poly a) where
    show = show' . reduce 

-- Folgende Funktionen werden verwendet um Polynome auf Normalform bringen zu
-- können. Polynome sind in Normalform, wenn der erste und der letzte Koeffizient
-- der Liste ungleich 0 sind. Das Null-Polynom hat entsprechend die leere Liste.
-- Beim Null-Polynom ist der Verschiebefaktor per Definition = 0.

-- Nullen vom Anfang und dem Ende einer Liste streichen.
trim :: (Eq a, Num a) => [a] -> [a]
trim = f . f
   where f = reverse . dropWhile (0 ==)

-- Entfernt Schritt für Schritt eine 0 vom Anfang der Liste. Entsprechend
-- veringert sich der Verschiebefaktor um 1 pro entfernter 0.
reduce2 :: (Eq a, Num a) => Poly a -> Poly a
reduce2 (Poly m []) =  (Poly 0 [])
reduce2 (Poly m (a:as)) =
    if a == 0 then 
        reduce (Poly (m-1) as) 
    else 
        (Poly m (a:as))

-- Bringt ein Polynom auf oben beschriebene Normalform. Dazu werden durch
-- "reduce2" die Nullen vom Anfang der Liste entfernt und anschließend durch
-- "trim" die am Ende.
reduce :: (Eq a, Num a) => Poly a -> Poly a
reduce (Poly m c) = (Poly n (trim d))
    where (Poly n d) = reduce2 (Poly m c)

-- Vergleicht zwei Polynome auf Gleichheit im zu starken Sinne, dass Verschiebe-
-- faktor und Koeffizientenliste gleich sein müssen.
eq2 :: Eq a => Poly a -> Poly a -> Bool
eq2 (Poly m c) (Poly n d) = (m == n) && (c == d) 

-- Erstellt einen String der das Polynom in lesbarer Form beschreibt. Es werden
-- die Fälle "Nullpolynom", "Monom" und "allgemeines Polynom" unterschieden.
-- "showX" erzeugt einen String der die Variable mit Exponent darstellt.
show' :: (Eq a, Num a, Show a) => Poly a -> String
show' (Poly m []) = "0"
show' (Poly m [a]) = show a ++ showX (-m)
show' (Poly m (a:as)) = (
    if a /= 0 then 
        show a ++ showX (-m) ++ " + "
    else "") 
    ++ show' (Poly (m-1) as)

showX :: Int -> String
showX e = if e == 0 then "" else if e == 1 then "X" else "X^" ++ (show e)

-- Ändert den Verschiebefaktor eines Polynoms. Entspricht der Multiplikation
-- mit einem normierten Polynom. Übergibt man der Funktion eine positive Zahl,
-- so erhöht sich der Grad des Polynoms um genau diese Zahl.
shift_poly :: Int -> Poly a -> Poly a
shift_poly s (Poly m c) = Poly (m-s) c

-- Diese Funktion bietet die Möglichkeit den Verschiebefaktor eines Polynoms
-- anzupassen ohne seinen Wert zu verändern. Das Ergebniss dieser Funktion ist
-- also äquivalent zur Eingabe.
lower_shift :: (Num a) => Int -> Poly a -> Poly a
lower_shift s (Poly m c) = 
    if s > m then
        Poly s ((replicate (s-m) 0) ++ c)
    else
        Poly m c

-- Addiert zwei Listen komponentenweise. Das Ergebnis hat die Länge der längeren
-- Eingabeliste. Die kürzere Liste wird dabei sozusagen mit Nullen aufgefüllt.
list_add :: (Num a) => [a] -> [a] -> [a]
list_add as [] = as
list_add [] bs = bs
list_add (a:as) (b:bs) = (a+b) : (list_add as bs)

-- Addiert zwei Polynome. Dazu wird das Polynom mit dem kleineren Verschiebe-
-- faktor so angepasst, dass die Verschiebefaktoren übereinstimmen (siehe 
-- "lower_shift"). Anschließend können die Koeffizientenlisten komponentenweise
-- addiert werden (siehe "list_add").
poly_add :: (Num a) => Poly a -> Poly a -> Poly a
poly_add (Poly m c) (Poly n d) =
    if m == n then
        Poly m (list_add c d)
    else
        poly_add (lower_shift k (Poly m c)) (lower_shift k (Poly n d))
    where k = max m n

-- Multipliziert zwei Listen miteinander. Dabei wird das Prinzip der Faltung
-- verwendent (also keine komponentenweise Multiplikation). Die Berechnung
-- geschieht rekursiv durch Ausnutzen des Distributivgesetzes.
list_mult :: (Num a) => [a] -> [a] -> [a]
list_mult as [] = []
list_mult as (b:bs) = list_add (map (*b) as) (0:(list_mult as bs))

-- Multipliziert zwei Polynome miteinander. Es genügt die Verschiebefaktoren
-- zu addieren und die Koeffizientenlisten zu multiplizieren (mit "list_mult")
poly_mult :: (Num a) => Poly a -> Poly a -> Poly a
poly_mult (Poly m c) (Poly n d) = Poly (m + n) (list_mult c d)

-- Multiplikation mit einem Skalar, also Multiplikation aller Koeffizienten mit
-- dem Skalar.
poly_smult :: (Num a) => a -> Poly a -> Poly a
poly_smult s (Poly m c) = Poly m (map (*s) c)