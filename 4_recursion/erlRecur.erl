-module(erlRecur).
-author("Kirk"). 
-version("1.0"). 
-export([fac/1, len/1, tail_len/1, tail_len/2, start/0]).

% recursive function  returning the factorial of the input integer
fac(N) when N > 0 -> N*fac(N-1);
fac(N) when N == 0 -> 1.

% recursive function  letting you know the length of the input list.
len([])->0;
len([_|T]) -> 1 + len(T).

% 위의 len 함수를 분석해보자
% len([_|T]) 는 1 + len(T)를 반환하는데, 여기서 len(T)가 호출되므로 call stack에 push 된다.
% 종료 조건이 될때까지 쌓이게 되고, 계산된 값을 반환하면서 stack에서 pop되게 된다. 

% Tail recursion은 이러한 stacking을 최소화하는 것을 목적으로 하고 있다.
% 아직까지는 len과 tail_len의 차이를 잘 모르겠다. 
% call stack에 push되는 횟수는 동일하지 않은가?
% 아 아니다 더 쌓이는 구나, 함수호출(len([_|T])도 쌓이고, 1+len(T)도 쌓이고, and so on.
% 단순하게 직접 스택 쌓아보면 감이 바로 올 것이다. 

% 읽어보자 
% In order to achieve this, we will need to hold an extra temporary variable as a parameter in our function.
% The aforementioned temporary variable is sometimes accumulator and acts as a place to store the results 
% of our computations as they happen in order to limit the growth of our calls.
% Acc를 활용한다는 것이구나. 매개변수하나를 더 사용해서 

tail_len(L) -> tail_len(L,0).
tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T, Acc+1).


start() ->
    X = fac(5),
    io:fwrite("~w~n", [X]),
    Y = [1,2,3,4],
    io:fwrite("~w~n", [len(Y)]).
