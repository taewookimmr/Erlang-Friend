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

### 2. 오류의 원격 처리 

* 얼랭 철학에서 중요한 오류의 원격 처리

### 3. 오류의 처리 상세

* 연결 
    * 두 프로세스 간에 오류의 확산 경로를 정의하는 것이지. 연결이란
    * 주어진 프로세스와 연괼되어 있는 일련의 프로세스를 그 프로세스의 연결 집합이라고 부른다.

* 종료 신호
    * 어떤 프로세스가 죽을 때 해당 프로세스가 생성하는 어떤 것.
    * 이 신호는 link set에 broadcasted 된다.
    * 종료 신호에는 그 프로세스가 왜 죽었는지 사유를 말해주는 인수가 들어있다.


* 시스템 프로그램
    * 어떤 프로세스가 비정상 종료 신호를 받으면, 그 프로세스가 시스템 프로세스가 아닌 이상, 그 프로세스 역시 죽는다.
    * to be continued


### 4. 종료 잡기 프로그래밍 관용법

* Pid = spawn(fun() -> ... end)
    * spawn을 사용한 병렬 프로세스를 생성하기
    * 띄운 프로세스(spawn 으로) 가 죽어도, 현재 프로세스(그 내부 코드에서 spawn이 있는)는 계속된다.

* Pid = spawn_link(fun() -> ... end)
    * 내가 생성한 프로세스가 비정상적으로 죽으면 나도 죽으련다.
    * 미리 종료를 잡도록 설정되어 있어서는 안 된다. ?

* 내가 생성한 프로세스가 죽으면 오류를 처리하고 싶다
    ```erlang
    % 어떤 프로그램을 구성하는 코드 내부임
    ...
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> ... end),
    ...
    loop(...).

    loop(State)->
        receive
            {'Exit', SomePid, Reason} ->
                dosomethingwith(Reason),
                loop(State1);
            ...
        end
    ```

### 5. 오류 처리 Primitive

* @spec spawn_link(Fun) -> Pid
    * 이것은 spawn과 똑같지만, 부모 자식 프로세스 간에 연결도 생성된다. 
    * 이것은 spawn과 link를 이어서 하는 것도 동일하지 않다. 띄우고 연결하는 도중에 죽을 수도 있기 때문에 다른 명령이라고 할 수 있다. 

* @spec process_flag(trap_exit, true)
    * 현재 프로세스를 시스템 프로세스로 만든다. 

* @spec link(Pid) -> true
    * 프로세스 Pid로 연결이 없을 때 연결을 생성, 연결은 대칭적 
    * Pid에 해당하는 프로세스가 없을 경우 nonproc 종료 예외가 발생 
    * 이미 연결되어 있을 경우, 그 호출은 무시

* @spec exit(Why) -> none()
    * 현재 프로세스를 사유 Why로 종료 시킨다. 
    * 이 때의 종료 신호와 Why를 link set에 broadcasting 한다.
        * 다만, 이 명령을 수행하는 절이 catch 문에 없는 경우에 한해서.

* @spec exit(Pid, Why) -> true
    * 종료 신호를 사유 Why와 함께 프로세스 Pid로 보낸다. 타겟을 특정하는 것이구나

* @spec erlang:monitor(process, Item) -> MonitorRef
    * Item은 Pid이거나 또는 프로세스의 등록된 이름 

### 6. 무정지 시스템은 어떻게 만드나?

* 무언가를 무정지로 만들려면, 적어도 컴퓨터 두 대는 필요
* 한 놈은 작업하고, 다른 한 놈은 지켜보고, 그러다가 잘못되면 이어받고
* worker-supervisor 모델이라고 한다.
* 이 모든 것을 가능케 해주는 것이 바로 link 명령 


### 7. 모니터

* link 연결은 대칭적, 그래서 프로그래밍이 종종 까다롭
* 상대방이 죽었을 때, 내가 죽지 않으려면 내가 시스템 프로세스면 되는데, 그러고 싶지 않을 때
* 모니터를 사용한다!

* 모니터는 비대칭적

### 8. 계속 살아 있는 프로세스

```erlang
keep_alive(Name,Fun)->
    register(Name, Pid = spawn(Fun)),
    on_exit(Pid, fun(_Why) -> keep_alive(Name, Fun) end.)
```

* 그런데 register와 on_exit 사이에서 프로세스가 죽는다면?
* race condition이 발생할 수 있다. 