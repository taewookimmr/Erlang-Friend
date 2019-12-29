-module(area_server).
-export([loop/0]).

loop()->
    receive 
        {rectangle, Width, Ht} ->
            io:format("area of rectangle is ~p~n", [Width* Ht]),
            loop()
end.
