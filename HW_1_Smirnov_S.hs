--data Term = IntConstant{ intValue :: Int }    
--            | Variable{ varName :: String }    
--            | BinaryTerm{ lhv :: Term, rhv :: Term } deriving(Show,Eq)

--1.1
data Operator =  Sum | Mul | Sub deriving(Show,Eq)-- <*> | <-> | <+>

data Term = IntConstant{ intValue :: Int }    
            | Variable{ varName :: String }  
			| UnaryTerm{ op::Operator, rhv::Term}
            | BinaryTerm{ lhv :: Term, op::Operator, rhv :: Term } deriving(Show,Eq)
			
(<+>) lhv rhv = BinaryTerm lhv Sum rhv
lhv <*> rhv = BinaryTerm lhv Mul rhv
(<->) lhv rhv = BinaryTerm lhv Sub rhv
--(-) rhv = UnaryTerm Sub rhv



replaceVar nameVar repTerm (IntConstant intValue)= IntConstant intValue 
replaceVar nameVar repTerm (Variable varName)= if nameVar == varName then repTerm else Variable varName
replaceVar nameVar repTerm (UnaryTerm op rhv)= UnaryTerm op (replaceVar nameVar repTerm rhv )
replaceVar nameVar repTerm (BinaryTerm lhv op rhv)=BinaryTerm (replaceVar nameVar repTerm lhv) op (replaceVar nameVar repTerm rhv)


--1.2
--реализация функции вычисления синуса по ряду Маклорена, угол х задаётся  в радианах! 
mysin :: Double -> Int -> Double
mysin x n = let fact x = if x <= 1 then 1 else x*fact(x-1) in 
		if n >= 0 then ((x^(2 * n + 1))*(-1)^n)/(fromIntegral $ fact(2 * n + 1)) + mysin x (n-1) else 0
		
		
--реализация алгоритма Евклида для наибольшего общего делителя двух чисел
nod :: Int->Int->Int 
nod x y = if x <= 0 || y <= 0 then 0
		  else if x > y then nod (x-y) y
		  else if x==y then x 
		  else nod (y-x) x 
		  
--реализация функции поиска  натурального числа, являющегося квадратом другого числа, между двумя заданными целыми числами
--На вход подаётся два натуральных числа ограничивающих поиск, в ответ выдаётся наибольшее из искомых чисел или же 0 в случае отсутствия такого 
quad x y = let hf y = ( floor $ sqrt $ fromIntegral y )^2 in if hf y > x && hf y < y then hf y 
															else  if hf y == y then quad x (y-1)
															else 0
 															