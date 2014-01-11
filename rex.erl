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

-module(rex).
-include_lib("eunit/include/eunit.hrl").
-export ([parse/1,eval/1,rexify/1,shuntingYard/3,stack/2]).



%DONE Evaluation funtions
eval({num, X}) -> X;
eval({unOp, _, Y}) -> -eval(Y);
eval({binOp, add, L, R}) -> eval(L) + eval(R);
eval({binOp, sub, L, R}) -> eval(L) - eval(R);
eval({binOp, divide, L, R}) -> eval(L) / eval(R);
eval({binOp, multiply, L, R}) -> eval(L) * eval(R).

rexify(S)->
	Tokens = lists:flatten([parse(S)]),
	RPNExpression = shuntingYard(Tokens, [], []),
	{ok, {num, Answer}, _} = evalRPN(RPNExpression, []),
	Answer.


%Take as input a String
parse([]) -> [];
parse([$~|T])->
	lists:append([{unOp, minus}], parse(T));
%Operands - if token is operand, add to parsed list
parse([$+|T])->
	lists:append([{binOp, add,2}], parse(T));
parse([$-|T])->
	lists:append([{binOp, sub,2}], parse(T));
parse([$*|T])->
	lists:append([{binOp, multiply,3}], parse(T));
parse([$/|T])->
	lists:append([{binOp, divide,3}], parse(T));
parse([$(|T])->
	lists:append([{bracket, left}], parse(T));
parse([$)|T])->
	lists:append([{bracket, right}], parse(T));
parse([H, H2|T]) when (H > 47) and (H < 58) and (H2 > 47) and (H2 < 58) ->
	{ok, numeger, Tail} = getnum([H,H2|T], []),
	lists:append([{num, numeger}],
	parse(Tail));
parse([H|T]) when (H > 47) and (H < 58) ->
	lists:append([{int, H-48}], parse(T));
%If token is a space just ignore it
parse([_WS|T])->
	parse(T).

%Extracts an number from a string and returns the remaining string
getnum([], Accum)->
	{ok, Accum, []};
getnum([H|T], Accum) when (H > 47) and (H < 58) ->
	numeger = Accum ++ [H],
	getnum(T, numeger);
getnum([H|T], Accum)->
	{ok, Accum, [H|T]}.



%Number, push to output queue
shuntingYard([{num, numeger} | T], Stack, OutputQueue)->
	erlang:display("shuntingYard Number\n"),
	% NewOutputQueue = [OutputQueue | {num, numeger}],
	NewOutputQueue =  OutputQueue ++ [{num, numeger}],
	shuntingYard(T, Stack, NewOutputQueue);

%Operator - When TokenOp <= StakOp precedence - pop off stack and put it in the output queue, put tokenOp on stack
shuntingYard([{binOp, TokenOp, Precedence} | T], [{binOp,StackOp,StackHeadPrec} | StackTail],  OutputQueue) when (Precedence =< StackHeadPrec) ->
	erlang:display("shuntingYard Operator\n"),
	%pop op off stack and put it in output queue
	shuntingYard(T, [{binOp, TokenOp, Precedence}] ++ [StackTail], OutputQueue ++ [{binOp,StackOp,StackHeadPrec}]);
%Operator - when token is op and stack head is op AND stackOp has > precedence - push TokenOp
shuntingYard([{binOp, TokenOp, Precedence} | T], [{binOp,StackOp,StackHeadPrec} | StackTail],  OutputQueue) ->
	erlang:display("shuntingYard Operator\n"),
	%push op to stack
	shuntingYard(T, [{binOp, TokenOp, Precedence},{binOp,StackOp,StackHeadPrec}] ++ StackTail, OutputQueue);
%Operator without operator on stack - push op to stack
shuntingYard([{binOp, TokenOp, Precedence} | T], Stack,  OutputQueue) ->
	erlang:display("shuntingYard Operator\n"),
	%push op to stack
	shuntingYard(T, [{binOp, TokenOp, Precedence}] ++ Stack, OutputQueue);

%Left bracket Push to stack
shuntingYard([{bracket,left} | T], Stack,  OutputQueue)->
	erlang:display("shuntingYard LB\n"),
	shuntingYard(T, [{bracket,left}] ++ Stack, OutputQueue);
%Rigth bracket
shuntingYard([{bracket,right} | T], Stack,  OutputQueue)->
	erlang:display("shuntingYard RB\n"),
	{NewTokens,NewStack,NewOutputQueue} = popFromStackUntilLeftBracketFound(T, Stack,  OutputQueue),
	shuntingYard(NewTokens,NewStack,NewOutputQueue);
%No more tokens left. Pop all from stack onto output queue
shuntingYard([],[StackHead | StackTail],OutputQueue)->
	erlang:display("shuntingYard No tokens left\n"),
	shuntingYard([], StackTail, OutputQueue ++ [StackHead]);
%Empty tokens and empty stack - return the output queue
shuntingYard([],[],OutputQueue)->
	OutputQueue.


popFromStackUntilLeftBracketFound(Tokens, [{bracket,left} | StackTail], OutputQueue)->
	{Tokens, StackTail, OutputQueue};
popFromStackUntilLeftBracketFound(Tokens, [StackHead | StackTail], OutputQueue)->
	popFromStackUntilLeftBracketFound(Tokens, StackTail, OutputQueue ++ [StackHead]).




evalRPN([],Stack)->
	stack(pop, Stack);
%If token is number, push to stack
evalRPN([{num, Value} | T], Stack)->
	erlang:display(Stack),
	evalRPN(T, [{num, Value}] ++ Stack);
%If operator
evalRPN([{binOp, Operator, _} | T], Stack)->
	% {ok, PoppedValue1, NewStack1} = stack(pop, Stack),
	% {ok, PoppedValue2, NewStack2} = stack(pop, NewStack1),
	{ok, Sum, NewStack3} = stack(Operator, Stack),
	{ok, NewStack4} = stack({push, Sum}, NewStack3),
	evalRPN(T, NewStack4).



stack({push, Value}, StackList)->
	{ok, [Value] ++ StackList};
stack(add, [{num, Value1},{num, Value2} | StackTail])->
	Sum = {num, Value1 + Value2},
	{ok, Sum, StackTail};
stack(sub, [{num, Value1},{num, Value2} | StackTail])->
	Sum = {num, Value1 - Value2},
	{ok, Sum, StackTail};
stack(divide, [{num, Value1},{num, Value2} | StackTail])->
	Sum = {num, Value2 / Value1},
	{ok, Sum, StackTail};
stack(multiply, [{num, Value1},{num, Value2} | StackTail])->
	case is_numeger(Value1) of
		false -> V1 = list_to_numeger(Value1);
		_ -> V1 = Value1
	end,
	case is_numeger(Value2) of
		false -> V2 = list_to_numeger(Value2);
		_ -> V2 = Value2
	end,
	Values = V1 * V2,
	Sum = {num, Values},
	{ok, Sum, StackTail};
stack(pop, [StackHead | StackTail])->
	{ok, StackHead, StackTail};
stack(pop, [])->
	{error, emptyStack}.


stack_test_()->
	[
		?_assert(stack({push, 5}, []) =:= {ok, [5]}),
		?_assert(stack({push, 5}, [5]) =:= {ok, [5,5]}),
		?_assert(stack({push, 2}, [5,5]) =:= {ok, [2,5,5]}),
		?_assert(stack(pop, [5,6,7]) =:= {ok, 5, [6,7]}),
		?_assert(stack(pop, [5]) =:= {ok, 5, []}),
		?_assert(stack(pop, []) =:= {error, emptyStack}),
		?_assert(stack(sub, [{num, 10},{num, 8}]) =:= {ok, {num,2}, []}),
		?_assert(stack(add, [{num, 10},{num, 8}]) =:= {ok, {num,18}, []}),
		?_assert(stack(multiply, [{num, 10},{num, 8}]) =:= {ok, {num,80}, []}),
		?_assert(stack(divide, [{num, 4},{num, 8}]) =:= {ok, {num,2.0}, []}),
		?_assert(stack(divide, [{num, 4},{num, 8} | [{num, 40}, {num, 9}, {binOp, add}]]) =:= {ok, {num,2.0}, [{num, 40}, {num, 9}, {binOp, add}]})

	].
