{-# LANGUAGE ExistentialQuantification #-}

data E = forall a. Eval a => E a
data EInt = EInt Int
data EAdd = EAdd E E
data EMul = EMul E E

class    Eval a    where eval :: a -> Int
instance Eval E    where eval (E x) = eval x
instance Eval EInt where eval (EInt x) = x
instance Eval EAdd where eval (EAdd x y) = eval x + eval y
instance Eval EMul where eval (EMul x y) = eval x * eval y
 
main = do
  let i = E(EInt 1)
  putStrLn $ "eval 1 = " ++ show (eval i)

  let add = E(EAdd (E(EInt 1)) (E(EInt 2)))
  putStrLn $ "eval 1 + 2 = " ++ show (eval add)

  let mul = E(EMul (E(EAdd (E(EInt 1)) (E(EInt 2)))) (E(EInt 111)))
  putStrLn $ "eval (1 + 2) * 111 = " ++ show (eval mul)
