-module(try_test).
-export([exgen/1,  demo1/0, demo2/0, demo3/0, catcher/1]).

exgen(1) -> a;
exgen(2) -> throw(a);
exgen(3) -> exit(a);
exgen(4) -> {'EXIT', a};
exgen(5) -> erlang:error(a).

demo1()->
    [catcher(I) || I <- [1,2,3,4,5]].

catcher(N) ->
    try exgen(N) of
        Val -> {N, normal, Val}
    catch
        throw : X -> {N, caught, thrown, X};
        exit : X -> {N, caught, exited, X};
        error: X -> {N, caught, error, X}
    end.


demo2() ->
    [{I, (catch exgen(I))} || I <- [1,2,3,4,5]].


% stack tracing
demo3() ->
    try exgen(5)
    catch
        error:X->
            {X, erlang:get_stacktrace()}
    end.