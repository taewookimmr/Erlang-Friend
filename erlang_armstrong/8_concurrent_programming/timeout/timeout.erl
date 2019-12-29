-module(timeout).
-export([pr/0]).

pr () ->
    receive
        {alarm, X} ->
                {alarm, 2*X}
    after 0 ->
        receive 
            Any ->
                pr()
        end
    end.

