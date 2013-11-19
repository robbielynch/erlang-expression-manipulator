-module(rex).
-export ([parse/1,eval/1]).



%Evaluation funtions
eval({int, X}) -> X;
eval({unOp, X, Y}) -> -eval(Y);
eval({binOp, add, L, R}) -> eval(L) + eval(R);
eval({binOp, sub, L, R}) -> eval(L) - eval(R);
eval({binOp, divide, L, R}) -> eval(L) / eval(R);
eval({binOp, multiply, L, R}) -> eval(L) * eval(R).

	
%Take as input a String
parse([]) -> [];
parse([H|T]) when H =:= '~' ->
	parse(T) ++ {unOp, minus}.
	% parse(T),
	% {bracket, left};
	% parse(T),
	% {bracket, right}.