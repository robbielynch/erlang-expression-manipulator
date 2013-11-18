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
-export([rexify/1]).

rexify(E) ->
	ExprWithoutSpaces = removeSpaces(E),
	recurseString(E).

removeSpaces(E) ->
	re:replace(E, "\\s+", "", [global,{return,list}]).

recurseString([]) ->
	finito;
recurseString([H|T]) ->
	erlang:display(H),
	recurseString(T).