-module(rex).
-export ([eval/1]).

eval({int, X}) -> X;
eval({unOp, X, Y}) -> eval(Y);
eval({binOp, add, L, R}) -> Answer = eval(L) + eval(R);
eval({binOp, sub, L, R}) -> Answer = eval(L) - eval(R);
eval({binOp, divide, L, R}) -> Answer = eval(L) / eval(R);
eval({binOp, multiply, L, R}) -> Answer = eval(L) * eval(R).

	
