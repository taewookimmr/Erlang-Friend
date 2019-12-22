-module(erlRecur2).
-export([dup/2, tail_reverse/2, start/0]).

dup(0, _)->[];
dup(N, Term) when N>0 ->
    io:fwrite("~w~n", [Term]),
    % [Term|dup(N-1, Term)]. % 왜 이런식의 코드를 작성하는 거지, pattern checking 할 것도 아닌 거 같은데.
    [Term|dup(N-1, Term)].

tail_reverse(L) -> tail_reverse(L, []).
tail_reverse([], Acc) -> Acc;
tail_reverse([H|T], Acc) -> tail_reverse(T, [H|Acc]).



start()->
    X= [1,2,3,4],
    dup(5,X),
    io:fwrite("~w~n", ["Make"]),
    Y = tail_reverse(X),
    io:fwrite("~w", [Y]).
