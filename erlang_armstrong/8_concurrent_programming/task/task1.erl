-module(task1).
-export([start/2]).

start(AnAtom, Fun) ->
    register(AnAtom, spawn(fun() -> loop(Fun) end)).

loop(Fun) ->
    receive
        Any ->
            Fun(),
            loop(Fun)
    end.

%% cli 명령어
%% erl
%% c(task1).
%% task1:start(alias, io:format("tested gogo") end).
%% alias ! something.
%% >> tested gogosomething