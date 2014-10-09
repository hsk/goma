{-# LANGUAGE ExistentialQuantification #-}

data EInt = EInt Int
data EAdd = forall a b. (Eval a, Eval b) => EAdd a b
data EMul = forall a b. (Eval a, Eval b) => EMul a b

class    Eval a    where eval :: a -> Int
instance Eval EInt where eval (EInt x)   = x
instance Eval EAdd where eval (EAdd x y) = eval x + eval y
instance Eval EMul where eval (EMul x y) = eval x * eval y

main = do
  let i = EInt 1
  putStrLn $ "eval 1 = " ++ show (eval i)

  let add = EAdd (EInt 1) (EInt 2)
  putStrLn $ "eval 1 + 2 = " ++ show (eval add)

  let mul = EMul (EAdd (EInt 1) (EInt 2)) (EInt 111)
  putStrLn $ "eval (1 + 2) * 111 = " ++ show (eval mul)
