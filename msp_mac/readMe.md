# 책 두 바퀴 돌리는 중
* 첫 번째 돌릴때는 윈도우 환경에서 공부
* 지금은 맥 환경에서 복습하기, 계속 타이핑 하면서 언어, 로직에 익숙해지자

## MacOS에서 erlang 환경 setting 하기

## Chapter3 복습

* werl : 윈도우 전용 명령어
    * start.bat 파일의 werl -args_file vm.args 대신
    * 그냥 터미널에 erl -args_file vm.args를 입력하면 된다.

* 코드 예

```erlang
init(_Type, Req, [])->
    {ok, Req, no_state}

%% _Type : _ 언더바의 의미 : 해당 변수는 그 값이 무엇이든 간에 신경 쓰지 않는다는 것을 의미한다.
%% 사용하지 않는 변수에 컴파일러의 warning을 피하고 싶을 때 사용한다.   
```