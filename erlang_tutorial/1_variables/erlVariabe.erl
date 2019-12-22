-module(erlVariables).
-import(io, [fwrite/1]).

-export([testVariable/0]).

% 번역
% 변수는 대문자로 시작해야 한다.
% erlang에서 변수는 immutable이다.
% 데이터 타입 복습 : number, boolean, atom, bit string, tuple, list, map

% 출력 부분 syntax를 봅시다.
% "~w"는 c언어의 "%d"에 대응된다고 볼 수 있나, 일단은 그렇게 사용되는 것 처럼 보인다. 아직까지는
% [1+1] 이와 같이 []에 넣어준다. 

% "~w" 말고 "~f~n"(소수점표현), "-e"(10거듭제곱 표현)
% "~"는 formatting이 있을 거란 것을 나타낸다.
% ~f-n : f는 실수를 나타낸다는 것이고, n은 소수점 몇 째 자리까지 나타낼 지 결정한다.
% ~e : 지수 표현을 위한 것이다. 


testVariable() -> 
    A = 1,
    B = 2,
    io:fwrite("~w", [A+B]).
    



