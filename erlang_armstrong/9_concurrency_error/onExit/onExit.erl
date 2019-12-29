-module(onExit).
-export([on_exit/2, test/0]).

on_exit(Pid, Fun)->
    spawn(fun() ->
        process_flag(trap_exit, true),
        link(Pid),
        receive
            {'EXIT', Pid, Why} ->
                Fun(Why)
        end
    end).

test()->
    F = fun() ->
        receive
            X -> list_to_atom(X)
        end
    end,
    Pid = spawn(F),
    G = fun(Why) ->
        io:format("~p died with:~p~n", [Pid, Why])
        end,
    on_exit(Pid, G),
    Pid ! hello.