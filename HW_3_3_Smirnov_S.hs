--3.3
newtype PSet a = PSet{ contains :: (a -> Bool) }
newtype PSet2 a = PSet2{ contains2 :: (a -> Bool) }
newtype PSet3 a = PSet3{ contains3 :: (a -> Bool) }
newtype PSet4 a = PSet4{ contains4 :: (a -> Bool) }
--newtype PSet5 a = PSet5{ contains5 :: (a -> Bool) }

--известно 5 базовых операций над множествами:
-- объединение,пересечение, разность, симметрическая разность и декартово произведение.
--Попробуем реализовать функцию принадлежности над итоговыми множествами
class Monoid a where 
 mempty :: a 
 mappend :: a -> a -> a 
 
--Объединение множеств
instance Monoid (PSet s) where 
 mempty = PSet(\a -> False)
 mappend (PSet set1) (PSet set2) =  PSet(\a -> (set1 a) || (set2 a))
 
--Пересечение
instance Monoid (PSet2 s) where 
 mempty = PSet2(\a -> False)
 mappend (PSet2 set1) (PSet2 set2) = PSet2(\a -> (set1 a) && (set2 a))
 
--Разность
instance Monoid (PSet3 s) where 
 mempty = PSet3(\a -> False)
 mappend (PSet3 set1) (PSet3 set2) = PSet3(\a -> (set1 a) && not(set2 a))

--Симметрическая разность
instance Monoid (PSet4 s) where 
 mempty = PSet4(\a -> False)
 mappend (PSet4 set1) (PSet4 set2) = PSet4(\a -> ((set1 a)|| (set2 a)) && not((set1 a) && (set2 a)))
 
--Декартово произведение (предполагает информацию о паре данных,в задании только одно)
--instance Monoid (PSet5 s) where 
-- mempty = PSet5(\a b -> False)
-- mappend (PSet5 set1) (PSet5 set2) = PSet5(\a b-> ((set1 a) && (set2 b)))

--
instance Functor PSet where
 fmap fun (PSet set1)= PSet (\a -> True)

instance Functor PSet2 where
 fmap fun (PSet2 set1)= PSet2 (\a -> False) 
 
