%%%-------------------------------------------------------------------
%%% @author Robbie <robbie.lynch@outlook.com>
%%% @copyright (C) 2013, Robbie Lynch
%%% @doc This module takes as an argument, one string, evaluates the
%%% 	 erlang expression and returns the value.
%%%
%%% @end
%%% Created :  28 Nov 2013 by Robbie <robbie.lynch@outlook.com>
%%%-------------------------------------------------------------------


%Parser		: Takes as input an expression and converts in into an appropriate internal representation; (25 Marks)
%Evaluator	: Takes as input the internal representation of an expression and returns the value of the expression; (25 Marks)
%Compiler	: Takes as input the internal representation of an expression and a stack program that returns the value of the expression; (25 Marks)
%Simulator	: Takes as input a stack program and executes it. (25 Marks)

-module(rob).
-export ([parse/1,eval/1,doit/1,wff/1,getTerm/1,parseExpr/1]).



%DONE Evaluation funtions
eval({int, X}) -> X;
eval({{unOp, _}, Y}) -> -eval(Y);
eval({{binOp, add}, L, R}) -> eval(L) + eval(R);
eval({{binOp, sub}, L, R}) -> eval(L) - eval(R);
eval({{binOp, divide}, L, R}) -> eval(L) / eval(R);
eval({{binOp, multiply}, L, R}) -> eval(L) * eval(R).

doit(S)->
	Tokens = lists:flatten([parse(S)]),
	Tree = parseExpr(Tokens),
	eval(Tree).
	% RPNExpression = shuntingYard(Tokens, [], []),
	% {ok, {int, Answer}, _} = evalRPN(RPNExpression, []),
	% Answer.


%Take as input a String
parse([]) -> [];
parse([$~|T])->
	lists:append([{unOp, minus}], parse(T));
%Operands - if token is operand, add to parsed list
parse([$+|T])->
	lists:append([{binOp, add}], parse(T));
parse([$-|T])->
	lists:append([{binOp, sub}], parse(T));
parse([$*|T])->
	lists:append([{binOp, multiply}], parse(T));
parse([$/|T])->
	lists:append([{binOp, divide}], parse(T));
parse([$(|T])->
	lists:append([{bracket, left}], parse(T));
parse([$)|T])->
	lists:append([{bracket, right}], parse(T));
parse([H, H2|T]) when (H > 47) and (H < 58) and (H2 > 47) and (H2 < 58) ->
	{ok, Integer, Tail} = getInt([H,H2|T], []),
	lists:append([{int, Integer}], 
	parse(Tail));
parse([H|T]) when (H > 47) and (H < 58) ->
	lists:append([{int, H-48}], parse(T));
%If token is a space just ignore it
parse([_WS|T])->
	parse(T).

%Extracts an integer from an string and returns the remaining string
getInt([], Accum)->
	{ok, Accum, []};
getInt([H|T], Accum) when (H > 47) and (H < 58) ->
	Integer = Accum ++ [H],
	getInt(T, Integer);
getInt([H|T], Accum)->
	{ok, Accum, [H|T]}.




parseExpr(Tokens)->
	{Tree,[]} = wff(Tokens),
	Tree.

wff([{unOp, minus} | Rest])->
	{Term, Remainder} = getTerm(Rest),
	{{{unOp, minus},Term},Remainder};
wff(Tokens)->
	{LeftTerm, Rem1} = getTerm(Tokens),
	case Rem1 of
		[{binOp,Type} | Rem2]->
			{RightTerm, Remainder} = getTerm(Rem2),
			{{{binOp, Type},LeftTerm, RightTerm},Remainder};
		_Else->
			{LeftTerm,Rem1}
	end.

getTerm([{int, Value} | Rest])->
	{{int, Value}, Rest};
getTerm([{bracket,left} | Rest])->
	{Tree, Remainder} = wff(Rest),
	[{bracket, right} | Tail] = Remainder,
	{Tree,Tail}.
