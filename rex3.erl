-module (rex3).
-export ([evalRPN/2,stack/2]).

evalRPN([],Stack)-> 
	stack(pop, Stack);
%If token is number, push to stack
evalRPN([{int, Value} | T], Stack)-> 
	erlang:display(Stack),
	evalRPN(T, [{int, Value}] ++ Stack);
%If operator
evalRPN([{binOp, Operator, Precedence} | T], Stack)->
	% {ok, PoppedValue1, NewStack1} = stack(pop, Stack),
	% {ok, PoppedValue2, NewStack2} = stack(pop, NewStack1),
	{ok, Sum, NewStack3} = stack(Operator, Stack),
	{ok, NewStack4} = stack({push, Sum}, NewStack3),
	evalRPN(T, NewStack4).
	

stack({push, Value}, StackList)->
	{ok, [Value] ++ StackList};
stack(add, [{int, Value1},{int, Value2} | StackTail])->
	Sum = {int, Value1 + Value2},
	{ok, Sum, StackTail};
stack(sub, [{int, Value1},{int, Value2} | StackTail])->
	Sum = {int, Value1 - Value2},
	{ok, Sum, StackTail};
stack(divide, [{int, Value1},{int, Value2} | StackTail])->
	Sum = {int, Value2 / Value1},
	{ok, Sum, StackTail};
stack(multiply, [{int, Value1},{int, Value2} | StackTail])->
	V1 = list_to_integer(Value1),
	V2 = Value2,
	Values = V1 * V2,
	Sum = {int, Values},
	{ok, Sum, StackTail};
stack(pop, [StackHead | StackTail])->
	{ok, StackHead, StackTail}.



%[{int,5},{int,3},{binOp,add,2},{int,"12"},{binOp,multiply,3},{int,3},{binOp,divide,3},[]]
%Answer Should be 32
