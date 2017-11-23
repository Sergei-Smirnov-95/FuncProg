
--2.2

foLdl fun b [] = b
--foLdl fun b (l:r) = fun (foLdl fun b r) l
foLdl fun b (l:r) = foLdl fun (fun b l) r

--foLdr :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
foLdr fun b [] = b
--foLdr fun b lst = fun (foLdr fun b (init lst)) (last lst)
--foLdr fun b lst = foLdr  fun (fun b (last lst)) (init lst)
foLdr fun b (l:r) =fun (foLdr fun b r) l

mAp fun lst = foLdr (\acum x ->(fun x) : acum) [] lst 
  
--flatMap :: (a -> [b]) -> [a] -> [b]  
flatMaP fun lst = foLdr (\acum x -> (fun x) ++ acum) [] lst

--concat :: [a] -> [a] -> [a]  ???
conCat lst = foLdr (\acum x -> (x ++ acum)) [] lst

--filter :: (a -> Boolean) -> [a] -> [a]
filTer fun lst = foLdr (\acum x -> if (fun x) then x : acum else acum ) [] lst

--maxBy :: (a -> Integer) -> [a] -> a  
maxBy fun (l:r) = foLdr (\acum x -> if (fun x)> (fun acum) then x else acum ) l r

--minBy :: (a -> Integer) -> [a] -> a  
minBy fun (l:r) = foLdr (\acum x -> if (fun x)< (fun acum) then x else acum ) l r

--reverse :: [a] -> [a]  
reverSe lst = foLdl (\acum x -> x : acum) [] lst

--позиция отсчитывается слева, начиная с 0 
--eleMentAt :: Integer -> [a] -> a  
eleMentAt num [] = error "Empty list!"
eleMentAt num (l:r) = snd $ foLdl(\(pos,val) head-> if pos < 1 then (pos,val) else (pos-1,head)) (num,l) r

--В функции поиска индекса отсчёт ведётся слева направо начиная от 0. В случае отсутствия элемента выводится размер списка+1
getFirst (a,b,c) = a
--indexOf :: String -> [String] -> Integer
inDexOf elem [] = error "Empty list!"
inDexOf elem (l:r) = getFirst$ foLdl(\(pos,val,write) head -> if val == head then (pos,val,2) else if write== 0 then (pos+1,val,0) else (pos,head,2)) (0,elem,0) (l:r)														