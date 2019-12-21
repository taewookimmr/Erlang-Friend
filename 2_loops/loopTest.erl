-module(loopTest).
-export([while/1, while/2, start/0]).

while(L) -> while(L, 0).
while([], _) -> 
    io:fwrite("~p", ["end of while"]);

while([H|T], Count) ->
    io:fwrite("~w", [Count]),
    io:fwrite("~w~n", [H]),
    while(T, Count+1).

start() -> 
    X=[0,1,2,3,5],
    while(X).


% Erlang is a functional programming language and what needs to be remembered about all functional programming lang is that
% they don't offer any constructs for loops. 
% Instead, functional programming depends on a concept called recursion

% 재귀 이야기가 나오는군 
% 직접적인 while문이 없다!


