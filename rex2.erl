-module (rex2).
-export ([shuntingYard/3,popFromStackUntilLeftBracketFound/3]).
%{bracket,left},{int,7},{binOp,add,2},{bracket,left},{int,6},{binOp,divide,3},{int,2},{bracket,right},{bracket,right}
%  (7+(6/2))

%number
shuntingYard([{int, Integer} | T], Stack, OutputQueue)->
	erlang:display("shuntingYard Number\n"),
	% NewOutputQueue = [OutputQueue | {int, Integer}],
	NewOutputQueue =  OutputQueue ++ [{int, Integer}],
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
%Empty tokens and empty stack
shuntingYard([],[],OutputQueue)->
	OutputQueue.


popFromStackUntilLeftBracketFound(Tokens, [{bracket,left} | StackTail], OutputQueue)->
	{Tokens, StackTail, OutputQueue};
popFromStackUntilLeftBracketFound(Tokens, [StackHead | StackTail], OutputQueue)->
	popFromStackUntilLeftBracketFound(Tokens, StackTail, OutputQueue ++ [StackHead]).


	
	
