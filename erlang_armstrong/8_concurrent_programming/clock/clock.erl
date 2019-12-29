-module(clock).
-export([start/2, stop/0]).

start(Time, Fun) ->
    register(clock, spawn(fun() -> tick(Time, Fun) end)).

stop() -> clock ! stop.

tick(Time, Fun)->
    receive
        stop ->
            void
    after Time -> 
        Fun(),
        tick(Time, Fun)
    end.

% console에서 
% erl
% c(clock).
% clock:start(500, fun() -> io:format("Tick ~p~n", [erlang:now()]) end).
% clock:stop().