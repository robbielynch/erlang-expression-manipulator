-module(rex).
-export ([parse/1,eval/1,p/1]).



%Evaluation funtions
eval({int, X}) -> X;
eval({unOp, X, Y}) -> -eval(Y);
eval({binOp, add, L, R}) -> eval(L) + eval(R);
eval({binOp, sub, L, R}) -> eval(L) - eval(R);
eval({binOp, divide, L, R}) -> eval(L) / eval(R);
eval({binOp, multiply, L, R}) -> eval(L) * eval(R).

p(S)->
	lists:flatten([parse(S)]).

%Take as input a String
parse([]) -> [];
parse([H|T]) when H =:= 126 ->
	lists:append([{unOp, minus}], parse(T));
%Operands
parse([H|T]) when H =:= 43 ->
	lists:append([{binOp, add}], parse(T));
parse([H|T]) when H =:= 45 ->
	lists:append([{binOp, sub}], parse(T));
parse([H|T]) when H =:= 42 ->
	lists:append([{binOp, multiply}], parse(T));
parse([H|T]) when H =:= 47 ->
	lists:append([{binOp, divide}], parse(T));
parse([H|T]) when H =:= 40 ->
	lists:append([{bracket, left}], parse(T));
parse([H|T]) when H =:= 41 ->
	lists:append([{bracket, right}], parse(T));
parse([H|T]) when (H > 47) and (H < 58) ->
	lists:append([{int, H-48}], parse(T)).

%numberBuilder([]) -> [];
% numberBuilder([H|T])  when (H > 47) and (H < 58) -> [H] ++ numberBuilder(T);
% numberBuilder(_) -> [].