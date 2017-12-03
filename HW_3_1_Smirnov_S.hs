
--3.1
data WeirdPeanoNumber = Zero | Succ (WeirdPeanoNumber ) | Pred (WeirdPeanoNumber )

instance Eq WeirdPeanoNumber where
 (==) Zero Zero = True
 (==) (Succ a) (Succ b) = a==b 
 (==) (Pred a) (Pred b) = a==b  
 (==) (Succ a) (Pred b) = a==(Pred(Pred b))
 (==) (Pred a) (Succ b) = b==(Pred(Pred a))
 (==) a Zero = False
 (==) Zero a = False

instance Ord WeirdPeanoNumber where
 (<=) Zero Zero = True
 (<=) Zero (Succ b) = True
 (<=) Zero (Pred b) = False
 
 (<=) (Pred a) (Pred b) = a<=b 
 (<=) (Succ a) (Succ b) =  a<=b 
 
 (<=) (Pred a) b = True
 (<=) (Succ a) b = False
 
 (<) Zero Zero = False
 (<) Zero (Succ b) = True
 (<) Zero (Pred b) = False
 
 (<) (Pred a) (Pred b) = a<b 
 (<) (Succ a) (Succ b) =  a<b 
 
 (<) (Pred a) b = True
 (<) (Succ a) b = False
instance Num WeirdPeanoNumber where
 (+) Zero b = b
 (+) a Zero = a
 (+) (Succ a) (Succ b) = Succ (Succ (a+b))
 (+) (Pred a) (Pred b) = Pred (Pred (a + b))
 (+) (Pred a) (Succ b) = a + b
 (+) (Succ a) (Pred b) = a + b
 
  --(-) Zero b = fromInteger(- toInteger b)
 (-) Zero Zero = Zero
 (-) Zero (Succ a)= Pred(Zero - a)
 (-) Zero (Pred a)= Succ(Zero - a) 
 (-) a Zero = a
 (-) (Succ a) (Succ b) = a - b
 (-) (Pred a) (Pred b) = a - b
 (-) (Pred a) (Succ b) = Pred (Pred a - b)
 (-) (Succ a) (Pred b) = Succ (Succ a - b) 
 
 (*) Zero b = Zero
 (*) a Zero = Zero
 (*) (Pred a) (Pred b)= Succ (a*b - a - b)
 (*) (Succ a) (Succ b)= Succ (a*b + a + b)
 (*) (Pred a) (Succ b)= Pred (a*b + a - b)
 (*) (Succ a) (Pred b)= Pred (a*b - a + b)
 
 --infixl 7 *
 --infix 6 +,-
 
 abs Zero = Zero
 abs (Pred a) = if a <= Zero then Succ( abs a) else a 
 abs (Succ a) = if Zero <= a then a else Succ( abs a)
 
 signum Zero = Zero
 signum (Pred a) = if a <= Zero then Pred(Zero) else  if a == (Succ Zero) then Zero else Succ Zero
 signum (Succ a) = if Zero <= a then Succ(Zero) else  if a == (Pred Zero) then Zero else Pred Zero
 
 fromInteger a | (a == 0) = Zero
               | (a < 0) = Pred ( fromInteger(a + 1))
               | (a > 0) = Succ ( fromInteger(a - 1))
               
 
 
instance Real WeirdPeanoNumber where
 
 toRational Zero = toRational 0 
 toRational (Succ a) = toRational a + 1
 toRational (Pred a) = toRational a - 1
 
 --fromRational
 
instance Enum WeirdPeanoNumber where 
 
 toEnum a |(a == 0) = Zero
          |(a > 0) = Succ( toEnum(a - 1))
          |(a < 0) = Pred( toEnum(a + 1))
 
 fromEnum Zero = 0
 fromEnum (Pred a) = fromEnum a - 1
 fromEnum (Succ a) = fromEnum a + 1
 
instance Integral WeirdPeanoNumber where
  
 toInteger Zero = 0
 toInteger (Pred a) = toInteger a - 1
 toInteger (Succ a) = toInteger a + 1
  
 quotRem a Zero = error "zero deriving"
 quotRem a b | (a>Zero && b>Zero) =  helpquotRem a b
             | (a<Zero && b<Zero) =  helpquotRem (Zero-a) (Zero-b)
             | (a>Zero && b<Zero) =  (Zero - fst (helpquotRem a (Zero-b)), Zero - snd (helpquotRem a (Zero-b)))
             | (a<Zero && b>Zero) =  (Zero - fst (helpquotRem (Zero-a) b), Zero - snd (helpquotRem (Zero-a) b))
             | otherwise = (Zero,Zero)
             where helpquotRem a b |(a==b)  = (Succ(Zero),Zero) 
                                   |(a > b) = (Succ(fst (helpquotRem (a-b) b)),snd (helpquotRem (a-b) b))
                                   |(a < b) = (Zero,a)

  
instance Show WeirdPeanoNumber where
 show a = show(toInteger a )
 
myPeanoTest = (Zero + fromInteger (-2))*(abs (toEnum (-3))) 

a = Zero 
b = (Pred a) 
c = (Pred b) 
d = (Succ a)
e = (Succ d) 

test1 = quotRem b c
test2 = quotRem c b
test3 = quotRem d c
test4 = quotRem c d
test5 = quotRem b e
test6 = quotRem e d 
test7 = quotRem b a
test8 = quotRem a b
--MyPeano