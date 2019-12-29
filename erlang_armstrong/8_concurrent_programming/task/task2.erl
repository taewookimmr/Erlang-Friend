-module(task2).
-export([start/3, start/1]).

start(N) ->
    Root = spawn(fun() -> loop() end),
    start(N-1, Root, Root).

start(N, Parent, Root) ->
    if 
        N > 1 ->
            Pid_new = spawn(fun() -> loop(Parent) end),
            start(N-1, Pid_new, Root);
        N =< 1 ->
            Pid_tail = spawn(fun() -> loop(Parent) end),
            Root ! {Pid_tail} % first trigger, from tail to root
    end.

loop() ->

    receive
        {Msg, Tail, Acc} ->
            io:format("received yes I am Root: ~p~n", [Msg]),
            Tail ! {"Loop On", Tail, Acc+1},
            loop();
        {Tail} ->
            Tail ! {"Loop Initiaed", Tail, 1}, % secondary trigger, from root to tail 
            loop()
        
    end.

loop(Parent) ->
    receive
        {Msg, Tail, Acc} ->
            Parent ! {Msg, Tail, Acc+1},
            io:format("received : ~p  ~p Accum = ~w~n", [Parent, Msg, Acc]),
            loop(Parent)
    end.
