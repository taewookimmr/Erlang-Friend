-module(server).
-export([loop/0, rpc/2]).

rpc(Pid, Request) ->
    Pid ! {self(), Request}, % self()는 client process의 pid에 해당한다.
    receive 
        {Pid,Response} ->
            Response
    end.

loop()->
    receive
        {From, {rectangle, Width, Ht}}->
            From ! {self(), Width * Ht},
            loop();
        {From, {circle, R}}->
            From ! {self(), 3.14159 * R * R},
            loop();
        {From, Other} ->
            From ! {self(), {error, Other}},
            loop()
    end.


% client-server 소통 방식 : erlang-message 전달 방식 사용
% sender는 자신의 응답 주소를 포함시켜서 message를 보내야 한다. 