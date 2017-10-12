--type synon
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n src goal tmp 
      | n = 1 src goal tmp = [(src, goal)]
      | hanoi (n-1) src tmp goal ++ hanoi 1 src goal tmp ++ hanoi (n-1) tmp goal src
