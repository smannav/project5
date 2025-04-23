module Password where

newtype PwdOp a = PwdOp (String -> (a, String))

-- Functor instance
instance Functor PwdOp where
  fmap f (PwdOp run) = PwdOp $ \s ->
    let (a, s') = run s
    in (f a, s')

-- Applicative instance
instance Applicative PwdOp where
  pure x = PwdOp $ \s -> (x, s)
  (PwdOp ff) <*> (PwdOp fa) = PwdOp $ \s ->
    let (f, s1) = ff s
        (a, s2) = fa s1
    in (f a, s2)

-- Monad instance
instance Monad PwdOp where
  return = pure
  (PwdOp run) >>= k = PwdOp $ \s ->
    let (a, s1)     = run s
        (PwdOp run2)= k a
    in run2 s1

setPassword :: String -> PwdOp ()
setPassword newPwd = PwdOp $ \_ -> ((), newPwd)

checkPassword :: String -> PwdOp Bool
checkPassword attempt = PwdOp $ \pwd ->
  (attempt == pwd, pwd)

runPwdOp :: PwdOp a -> a
runPwdOp (PwdOp run) = fst (run "")





