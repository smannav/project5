import Ast

test :: (Expr -> Float) -> Expr -> Float
test f = f

main :: IO ()
main = do
    print $ test Ast.eval test1
    print $ test Ast.eval test2
    print $ test Ast.eval test3
    print $ eq test1 test1
    print $ eq test1 test2
