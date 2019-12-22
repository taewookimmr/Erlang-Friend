% hello world program
-module(helloworld).
-import(io, [fwrite/1]).
-export([start/0]).
-export([add/0]).
-export([typeTest/0]).

start() ->
    fwrite("Hello World\n").

add() ->
    io:fwrite("~w", [1+1]).

typeTest() ->
    Bin1 = <<10,20>>,
    X = binary_to_list(Bin1),
    io:fwrite("~w", [X]),

    P = {john, 24, {june, 25}},
    io:fwrite("~w", [P]),
    io:fwrite("~w", [tuple_size(P)]),

    M1 = #{name=>john, age=>25},
    io:fwrite("~w", [M1]),
    io:fwrite("~w", [map_size(M1)]).


    