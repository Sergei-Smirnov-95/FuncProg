
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
instance Monoid (ReverseList l) where
 mempty = RNil
 mappend RNil lst= lst
 mappend lst RNil = lst
 mappend lst1 lst2 = fromList $ (toList lst1) : (toList lst2)

--

instance Functor ReverseList where 
 fmap fun RNil = RNil
 fmap fun (RCons tail head) = RCons (fmap fun tail) (fun head)
 
--
