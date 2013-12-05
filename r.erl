%%%-------------------------------------------------------------------
%%% @author Joseph kehoe <josephkehoe@joseph-kehoes-imac.local>
%%% @copyright (C) 2013, Joseph kehoe
%%% @doc
%%% Sample Erlang file showing some parsing
%%% @end
%%% Created :  4 Dec 2013 by Joseph kehoe <josephkehoe@joseph-kehoes-imac.local>
%%%-------------------------------------------------------------------

%% @TODO Finish writing the documentation using edoc and unit tests using eunit.
-module(resolution).
-include_lib("eunit/include/eunit.hrl").
-export([parse/1,parseExpr/1,tokenize/1,wff/1,getTerm/1,convert/1]).




parse(String)->
    TokenList=tokenize(String),
    Tree=parseExpr(TokenList),
    Tree.
%    {_,_CNF}=convert(Tree).


%% @spec tokenize(X::string()) -> [term()]
tokenize("")->
    [];
tokenize([$(|Rest]) ->
    [{symbol,lbracket}|tokenize(Rest)];
tokenize([$)|Rest]) ->
    [{symbol,rbracket}|tokenize(Rest)];
tokenize([$&|Rest]) ->
    [{binaryOp,andOp}|tokenize(Rest)];
tokenize([$^|Rest]) ->
    [{binaryOp,orOp}|tokenize(Rest)];
tokenize([$~|Rest]) ->
    [{unaryOp,notOp}|tokenize(Rest)];
tokenize([$=,$>|Rest]) ->
    [{binaryOp,impliesOp}|tokenize(Rest)];
tokenize([$<,$=,$>|Rest]) ->
    [{binaryOp,equivalenceOp}|tokenize(Rest)];
tokenize([First|Rest]) when (First >= $a andalso First =<$z) ->
    [{prop,First}|tokenize(Rest)];
tokenize([_WS|Rest]) ->
    tokenize(Rest).


parseExpr(TokenList)->
    {Tree,[]}=wff(TokenList),
    Tree.

wff([{unaryOp,notOp}|Rest])->
    {Term,Remainder}=getTerm(Rest),
    {{{unaryOp,notOp},Term},Remainder};

wff(TokenList) ->
    {LeftTerm,Rem1}=getTerm(TokenList),
    case Rem1 of  
	[{binaryOp,Type}|Rem2] ->
	   {RightTerm,Remainder}=getTerm(Rem2),
	   {{{binaryOp,Type},LeftTerm,RightTerm},Remainder};
	_Else ->
	    {LeftTerm,Rem1}
    end.


getTerm([{prop,P}|Rest])->
    {{prop,P},Rest};

getTerm([{symbol,lbracket}|Rest]) ->
    {Tree,Remainder}=wff(Rest),
    [{symbol,rbracket}|Tail]=Remainder,
    {Tree,Tail}.

convert({{binaryOp,equivalenceOp},Left,Right})->
    convert({{binaryOp,andOp},{{binaryOp,impliesOp},Left,Right},{{binaryOp,impliesOp},Right,Left}});

convert({{binaryOp,impliesOp},Left,Right})->
    convert({{binaryOp,orOp},{{unaryOp,notOp},Left},Right});
    
convert({{unaryOp,notOp},{unaryOp,notOp,Wff}}) ->
    convert(Wff);

convert({{unaryOp,notOp},{binaryOp,andOp,Left,Right}}) ->
    convert({binaryOp,orOp,{unaryOp,notOp,Left},{unaryOp,notOp,Right}});

convert({{unaryOp,notOp},{binaryOp,orOp,Left,Right}}) ->
    convert({binaryOp,andOp,{unaryOp,notOp,Left},{unaryOp,notOp,Right}});


convert({{binaryOp,andOp},Left,Right})->
    {convert(Left),convert(Right)};

convert({prop,P})->
    {prop,P}.


%%Tests go here
simpleParse_test()->
    {binaryOp,andOp,{prop,"a"},{prop,"b"}}=tokenize("a&b").
anotherTest_test() ->
    tokenize("(a&b)^c").



