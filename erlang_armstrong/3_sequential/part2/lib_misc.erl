-module(lib_misc).
-export([pythag/1, perms/1, test/1]).

pythag(N) ->
    [{A,B,C} || 
        A<- lists:seq(1,N),
        B<- lists:seq(1,N),
        C<- lists:seq(1,N),
        A+B+C =< N,
        A*A+B*B =:= C*C
    ].

perms([]) -> [[]];
perms(L) -> [[H|T] || H <- L, T <- perms(L--[H])].

test([])->[];
test(L) -> [T || H <- L, T <- L--[H]].