-module(timeout).
-export([pr/0]).

pr () ->
    receive
        {alarm, X} ->
                io:fwrite("alarm ok, and after is not finally~n"),
                {alarm, 2*X}
    after 0 ->
        receive 
            Any ->
                io:fwrite("timeout-0~n"),
                Any,
                pr()
        end
    end.


%% 테스트 명령어
%% erl
%% c(timeout).
%% Pid = spawn(fun timeout:pr/0).
%% Pid ! Pid ! {alarm, 10}.
%% 이렇게 입력했을 경우, timout-0\n, alarm ko, and after is not finally가 출력된다
%% 책에 있는 예제 코드에서는 after의 body에 pr()을 재귀적으로 사용하지 않았는데,
%% 위와 같이 넣어줘야 의미가 있지 않을까? 잘 모르겠네
