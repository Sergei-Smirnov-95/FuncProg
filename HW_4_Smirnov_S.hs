--4
import Control.Applicative
data FunMonad a = FunMonad{ fun :: ()->a}

instance Functor FunMonad where
 --применяем функцию к результату, полученному из монады, результат заворачиваем в монаду
 fmap function (FunMonad monad) = FunMonad( \() ->function( monad ()))
  
instance Applicative FunMonad where
 --заворачиваем в монаду аргумент
 pure monad = FunMonad (\() -> monad)
 --Применяем функцию из а к аргументу из б и заворачиваем в монаду
 (FunMonad a) <*> (FunMonad b) = FunMonad (\() -> (a ()) $ (b ())) 
 
--для реализации необходимо реализовать Functor, Applicative
instance Monad FunMonad where
 --заворачиваем в монаду аргумент
 return monad = FunMonad (\() -> monad)
 --применяем принимаемую функцию к аргументу, полученному из функции
 monad >>= func = func ( (fun monad) ())
 
instance (Show m) => Show (FunMonad m) where
  show (FunMonad monad) = "data FunMonad: " ++ (show $ monad ())
  
-----------------Tests---------------------
mon1 = FunMonad (\ () -> 2 )
mon2 arg = FunMonad (\ () -> arg + 2)
mon3 = FunMonad (\ () -> "It is test2 for monad")
mon4 arg = FunMonad (\ () -> show arg)

test1 = mon1 >>= mon2
test2 = mon3 >>= mon4