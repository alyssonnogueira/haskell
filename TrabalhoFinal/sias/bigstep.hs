data E = Num Int
		|Soma E E
		|Mult E E
		deriving(Eq, Show)
		
data B = TRUE
		|FALSE
		|Not B
		|And B B
		|Or B B

bse:: E->Int
bse (Num n) = n
bse (Soma e1 e1) = (bse e1)+(bse e2)
bse (Mult e1 e2) = (bse e1)*(bse e2)

bsb:: B->Bool
bsb TRUE = True
bsb FALSE = False
bsb (Or b1 b2)
	|bsb b2 = True
	|Otherwise = bsb b2
