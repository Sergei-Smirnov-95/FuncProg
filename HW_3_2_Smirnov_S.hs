--3.2
data ReverseList a = RNil | RCons (ReverseList a) a 
-- 

instance (Show a) => Show (ReverseList a) where
 show RNil = "Empty list"
 show (RCons RNil head) = show head
 show (RCons tail head) = show tail ++ "," ++ show head
-- 

instance (Eq a) => Eq(ReverseList a) where
 (==) RNil RNil = True
 (==) (RCons tail1 head1) (RCons tail2 head2) = if head1==head2 then tail1 == tail2 else False
 (==) lst1 lst2 = False                                                                         --RNil+RCons
-- 

helptoLi RNil = []
helptoLi (RCons tail head) = if tail == RNil then head:[]  else head:helptoLi(tail)

toList lst = reverse ( helptoLi lst )
-- 

instance (Ord a) => Ord(ReverseList a) where
 (<=) RNil RNil = True
 (<=) lst1 lst2 = (toList lst1) <= (toList lst2)
-- 

helpfromLi [] = RNil
helpfromLi (h:t) = if t == []  then RCons RNil h else RCons (helpfromLi t) h

fromList lst = helpfromLi ( reverse lst ) 

--
class Monoid a where 
 mempty :: a 
 mappend :: a -> a -> a 
 
instance Monoid (ReverseList a) where
 mempty = RNil
 mappend RNil lst2 = lst2
 mappend lst1 RNil = lst1
 --mappend lst1 lst2 = fromList ( (toList lst1) : (toList lst2))
 mappend lst1 (RCons tail2 head2) = RCons (mappend lst1 tail2) head2
 
instance Functor ReverseList where 
 fmap fun RNil = RNil
 fmap fun (RCons tail head) = RCons (fmap fun tail) (fun head)
 
---------------Tests-----------------------
a = fromList [1,2,3,4]
b = fromList [5,6,7,8]
test = mappend a b
