--Реализуйте структуру данных "бинарное дерево поиска" для целых чисел без балансировки. Реализация включает функции:
--
--Добавления элемента: insert :: BinaryTree -> Integer -> BinaryTree
--Удаления элемента: remove :: BinaryTree -> Integer -> BinaryTree
--Создания пустого дерева: emptyTree :: BinaryTree
--Поиска элемента в дереве: containsElement :: BinaryTree -> Integer -> Bool
--Поиска в дереве наименьшего элемента, который больше или равен заданному: nearestGE :: BinaryTree -> Integer -> Integer
--Создания дерева из списка: treeFromList :: [Integer] -> BinaryTree
--Создания списка из дерева: listFromTree :: BinaryTree -> [Integer]

--Операторы insert и remove должны поддерживать цепочки вызовов в инфиксной форме           

--2.1

data BinaryTree = EmptyTree
                | Leaf Integer
                | Node Integer BinaryTree BinaryTree deriving(Show,Eq)
                

insert :: BinaryTree -> Integer -> BinaryTree
insert EmptyTree ins = Leaf ins
insert (Leaf l) ins |(l < ins) = Node l (Leaf ins) EmptyTree
                    |(l > ins) = Node l EmptyTree (Leaf ins)
                    |(l == ins) = Leaf l
insert (Node c l r) ins | (c == ins) = Node c l r
                        | (c > ins) = Node c l (insert r ins)
                        | (c < ins) = Node c (insert l ins) r  

 
--Алгоритм удаления основан на поиске минимального элемента(среди больших по значению) и замещении им удаляемого. 
--При этом, новая нода формируется из найденного элемента(вершина) и двух поддеревьев с удалённым замещаемым элементом-листом.
delMin (Node c l EmptyTree) = l
delMin (Leaf l) = EmptyTree
delMin (Node c l r) = Node c l (delMin r)
--                        
searchMin (Node c l EmptyTree) = c
searchMin (Leaf l) = l
searchMin (Node c l r) = searchMin r 
--                                  
mydel l EmptyTree = l
mydel EmptyTree r = r
mydel l r = Node (searchMin l) (delMin l) r 
--
remove :: BinaryTree -> Integer -> BinaryTree
remove EmptyTree _ = EmptyTree
remove (Leaf l) rem | (l == rem) = EmptyTree
                    | otherwise = Leaf l
remove (Node c l r) rem | (rem == c) =  mydel l r
                        | (rem < c) = Node c l (remove r rem)
                        | (rem > c) = Node c (remove l rem) r 
                        

emptyTree :: BinaryTree
emptyTree = EmptyTree

containsElement :: BinaryTree -> Integer -> Bool
containsElement EmptyTree _ = False
containsElement (Leaf l) y | (l == y) = True
                           | otherwise = False
containsElement (Node c l r) y | (c == y) = True
                               | (c < y) = containsElement l y
                               | (c > y) = containsElement r y
                               


helpnearestGE EmptyTree fin = fin - 1 
helpnearestGE (Leaf l) fin = l
helpnearestGE (Node c l r) fin |(c == fin) = c 
                           |(c < fin) = if helpnearestGE l fin < fin then fin - 1 else helpnearestGE l fin
                           |(c > fin) = if helpnearestGE r fin < fin then c else helpnearestGE r fin  
                               
nearestGE :: BinaryTree -> Integer -> Integer
nearestGE EmptyTree _ = error "Empty Tree"
nearestGE (Leaf l) fin = if l < fin then error "have not element" else l
nearestGE (Node c l r) fin  |(fin== c) = fin
                            |(c < fin) = if helpnearestGE l fin < fin then error "have not element" else helpnearestGE l fin
                            |(c > fin) = if helpnearestGE r fin < fin then c else helpnearestGE r fin


--treeFromList с вспомогательной функцией, необходимой для формирования дерева из пустого
treehelper t (l:r) = treehelper (insert t l) r
treehelper t [] = t                       
    
treeFromList :: [Integer] -> BinaryTree
treeFromList [] = EmptyTree
treeFromList (l:r) = treehelper emptyTree (l:r)


listFromTree :: BinaryTree -> [Integer]
listFromTree EmptyTree = []
listFromTree (Leaf l) = [l]
listFromTree (Node c l r) = (listFromTree l) ++ [c] ++ (listFromTree r)
 															