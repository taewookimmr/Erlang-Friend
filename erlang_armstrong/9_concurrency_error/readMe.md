## Chapter 9. 병행 프로그램과 오류

### Intro

* Link, Exit signal, System process라는 개념을 알아야 한다.

### 1. 프로세스 연결하기

* 어느 한 프로세스가 다른 프로세스의 상태를 알고 싶다면 BIF link를 사용한다
* 시스템 프로세스, 시스템 프로세스에 연결된 프로세스가 어떤 사유로 종료되면, 시스템 프로세스는 자동으로 종료되지 않는다. 그 대신 시스템 프로세스는 종료 신호를 받아, 그 신호를 잡아서 처리할 수 있다.

* on_exit Handler 작성 [Code](./onExit/onExit.erl)

```erlang

on_exit(Pid, Fun)->
    spawn(fun() ->
        process_flag(trap_exit, true),
        link(Pid),
        receive
            {'EXIT', Pid, Why} ->
                Fun(Why)
        end
    end).

```

```erlang
% 설명 
% spawn 에서 띄운 process_flag(trap_exit, true)는 해당 프로세스를 시스템 프로세스로 전환, 그래 I am ready to hear you,
% link(Pid) 매개변수로 받은 Pid, 즉 다른 프로세스와 연결한다. yeah you

```
