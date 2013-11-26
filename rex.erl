%%%-------------------------------------------------------------------
%%% @author Robbie <robbie.lynch@outlook.com>
%%% @copyright (C) 2013, Robbie Lynch
%%% @doc This module takes as an argument, one string and evaluates the
%%% 	 erlang expression.
%%%
%%% @end
%%% Created :  8 Oct 2013 by Robbie <robbie.lynch@outlook.com>
%%%-------------------------------------------------------------------
-module(rex).
-export ([parse/1,eval/1,p/1,toParseTree/1,stack/2]).



%DONE Evaluation funtions
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
parse([H, H2|T]) when (H > 47) and (H < 58) and (H2 > 47) and (H2 < 58) ->
	{ok, Integer, Tail} = getInt([H,H2|T], []),
	lists:append([{int, Integer}], 
	parse(Tail));
parse([H|T]) when (H > 47) and (H < 58) ->
	lists:append([{int, H-48}], parse(T)).


%Extracts an integer from an string and returns the remaining string
getInt([], Accum)->
	{ok, Accum, []};
getInt([H|T], Accum) when (H > 47) and (H < 58) ->
	Integer = Accum ++ [H],
	getInt(T, Integer);
getInt([H|T], Accum)->
	{ok, Accum, [H|T]}.



% convertInfixToPrefix([], Structure)->
% 	[];
% convertInfixToPrefix([{unOp, minus}|Tail], Structure)->
% 	{unOp}
% convertInfixToPrefix([{binOp, Operand}|Tail], Structure)->
% 	NewStructure = Structure ++ {binOp, Operand, convertInfixToPrefix(Tail)}.

toParseTree([])->
	{};
toParseTree([{unOp, minus}|T]) ->
	{unOp, minus, toParseTree(T)};
toParseTree([{unOp, add}|T])->
	{binOp, add, toParseTree(T)};
toParseTree([{unOp, sub}|T])->
	{binOp, sub, toParseTree(T)};
toParseTree([{unOp, divide}|T])->
	{binOp, divide, toParseTree(T)};
toParseTree([{unOp, multiply}|T])->
	{binOp, multiply, toParseTree(T)};
toParseTree([{bracket, left}|T]) ->
	toParseTree(T);
toParseTree([{bracket, right}|T]) ->
	toParseTree(T);
toParseTree([{int, Number}|T])->
	{int, Number}.

% [{unOp,minus},{bracket,left},{int,7},{binOp,add},{bracket,left},{int,6},{binOp,divide},{int,2},{bracket,right},{bracket,right}]

stack({push, Value}, StackList)->
	[Value | StackList];
stack({add}, [Stack1,Stack2 | StackTail])->
	Sum = Stack1 + Stack2,
	[Sum | StackTail];
stack({sub}, [Stack1,Stack2 | StackTail])->
	Sum = Stack1 - Stack2,
	[Sum | StackTail];
stack({divide}, [Stack1,Stack2 | StackTail])->
	Sum = Stack1 / Stack2,
	[Sum | StackTail];
stack({multiply}, [Stack1,Stack2 | StackTail])->
	Sum = Stack1 * Stack2,
	[Sum | StackTail];
stack({ret}, [Stack1,Stack2 | StackTail])->
	Sum = Stack1 + Stack2,
	[Sum | StackTail];
stack({pop}, [StackHead | _])->
	erlang:display(StackHead).

% pop([Head | StackList])->
% 	{ok, Head, StackList};		
% push(Value, StackList)->
% 	{ok, [Head | StackList]}.



