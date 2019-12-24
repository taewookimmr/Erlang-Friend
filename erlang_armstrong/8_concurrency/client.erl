-module(client).
-export([area/2]).

area(Pid, What)->
    rpc(Pid, What).

    rpc(Pid, Request) ->
        Pid ! {self(), Request},
        receive 
            {Pid, Response}->
                Response
        end.
        % rpc(Pid, Request).