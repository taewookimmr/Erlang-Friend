## Chapter 8. 병행 프로그래밍

### Intro

* 프로세스는 얼랭 함수를 평가할 수 있는 독립적인 가상 기계
* 얼랭에서 프로세스는 운영체제가 아닌 프로그래밍 언어에 속한다.
* 얼랭 프로세스의 특성
    * 생성, 제거 빠름
    * proc간 message 전송 빠름
    * 모든 os에 proc이 똑같은 방식으로 동작
    * 많은 수의 proc 가질 수 있다.
    * no memory sharing, definitly independent
    * pure message passing language


### 1. 병행성 Primitive

* Pid = spawn(Fun)
    * Fun을 평가하는 새 병행 프로세스를 생성한다
    * 이 프로세스는 호출자와 병렬로 실행된다. 
    * Pid는 프로세스로 메시지를 보낼 때 사용

* Pid ! Message 
    * 식별자가 Pid인 프로세스로 메시지를 보낸다.
    * 비동기로 전송된다. 즉, 전송하는 측은 응답을 기다리지 않고 하던거 한다.
    * 해당 명령은 Message 자체를 반환한다.
        * Pid1 ! Pid2 ! Pid3 ! M 명령은 이 프로세스들에게 모두 명령 M을 보내겠다는 것을 뜻한다.

* receive ... end 
    * 프로세스로 전송된 메시지를 받는다
    ```erlang
    receive
        Pattern1 [when Guard1] ->
            Expression1;
        Pattern2 [when Guard2] ->
            Expression2;
    ```

### 2. 클라이언트 서버 개론

* 클라이언트-서버 아키텍처는 얼랭의 중심 
* 클라이언트는 언제나 어떤 요청을 서버로 전송하여 계산을 시작하고, 서버는 이에 응해 계산을 처리하고 클라이언트로 응답을 전송한다.

* 일반적으로 최초로 요청을 보내는 프로세스를 클라이언트라고 부르고, 그 요청을 받아서 응답을 보내는 프로세스를 서버라고 한다. 


### 3. 프로세스 생성 관련

* [프로세스 생성 시간 관련 코드](./creatingtime/processes.erl)

    * cpu time ? wall-clock ? stack exchange 글 퍼옴
   
        The term 'user CPU time' can be a bit misleading at first. To be clear the total time (real CPU time), is the combination of the amount of time the CPU spends performing some action for a program and the amount of time the CPU spends performing system calls for the kernel on the program's behalf. When a program loops through an array, it is accumulating user CPU time. Conversely, when a program executes a system call such as exec or fork, it is accumulating system CPU time

        Wall clock time is the actual time taken by a computer to complete a task. It is the sum of three terms: CPU time, I/O time, and the communication channel delay (e.g. if data are scattered on multiple machines). In contrast to CPU time, which measures only the time during which the processor is actively working on a certain task, wall time measures the total time for the process to complete. The difference between the two consists of time that passes due to programmed delays or waiting for resources to become available.

    * 생성할 수 있는 프로세스의 최대값 : erlang:system_info(process_limit)가 반환하는 값으로 알 수 있음

        * 그 한계치를 초과해서 프로세스를 생성하고 싶다면, erl 에뮬레이터 시작 시, +P 숫자를 입력해주면 된다
        ```
        erl +P 500000
        ```
    * 특정 수치 이상의 프로세스를 생성하려고 하면, Paging이 발생하게 되는데, 이는 컴퓨터의 physical memor가 충분하지 않다는 신호이다.

### 4. 타임아웃이 있는 receive

* receive 문이 계속 대기 상태에 놓일 수 도 있다.
    * 프로그램에 논리적 오류가 있는 경우
    * 상대방이 죽었을 수도 있고

* 이런 문제를 피하는 방법 ; timeout을 추가한다. receive문에

    ```erlang
    receive 
        Pattern1 [when Guard1] ->
            Expression1;
        Pattern2 [when Guard2] ->
            Expression2;
        ...
    after Time ->
        Expression
    end
    ```

    * Time에는 millisecond에 해당하는 수치를 설정한다.
    
* 타임아웃만 있는 receive

    * 이 방식으로 소위 sleep 효과를 줄 수 있다.
    ```erlang
    sleep(T)->
        receive
        after Time ->
            True
        end.
    ```

* 타임아웃이 0인 receive

    * 타임아웃 본문이 즉각 실행된다. _Any를 이용하여 어떤 프로세스의 메일 박스 안에 있는 메시지들을 모두 완전히 비우는 함수를 정의할 수 있다.
    ```erlang
    flush_buffer() ->
        receive
            _Any ->
                flush_buffer()
        after 0 ->
            True
        end.
    ```
    * after 0이 없어도 되는거 아닌가? : 없다면, flush_buffer 함수는 메일박스가 텅 비었더라도 영원히 멈춰서서는 반환되지 않을 것이다.

    * 우선 수신, priority receive도 구현할 수 있다.
    ```erlang
    priority_receive() ->
        recevie
            {alarm, X} ->
                {alarm, X}
        after 0->
            receive
                Any ->
                    Any
            end
        end.
    ```
    * 아직 이해가 다 되지는 않는다. 
    *  after 0-> 다음
    Any -> Any가 아니라 Any -> priority_receive()로 해야하지 않나? 한번 직접 테스트 해보도록 
    * [Timeout-0 Code](./timeout/timeout.erl)

* 타임아웃 값이 무한인 receive

    * 타이머를 만들어 보자 [Timer](./timer/stimer.erl)


### 5. 선택적 수신

* send는 메시지를 프로세스의 메일박스로 보낸다. 
* receive는 그 메일바그에서 메시지의 제거를 시도한다. 
* 메일 박스는 프로그램에서 receive 문을 평가할 때만 검사된다. 

* receive 문의 작동 순서 
    * receive 문에 진입하면 타이머가 시작된다. (after 영역이 있는 경우)

    * 메일박스에서 첫 번째 메시지를 가져와 Pattern1, Pattern2 등과 매치를 시도한다.
        * 만일 매치가 성공되면 해당 메시지가 메일박스에서 제거된다. 

    * 메일박스의 첫 번째 메시지가 어떤 패턴과도 매치되지 않으면, 이 메시지는 메일박스에서 제거되고, 저장 큐에 들어간다. 그 다음으로 두 번째 메시지가 시도된다. 이 과정은 매치되는 메시지를 발견하거나 또는 메일 박스 안에 있는 모든 메시지를 조사할 때까지 반복

    * 메일박스 안의 어떠한 메시지도 매치되지 않으면 프로세스는 suspend 상태로 들어간다. supsend 상태에서 새 메시지가 메일박스로 들어오는 것에 대비해, 재조정될 것이다. 

    * 이 상태에서 새 메시지가 들어왔는데 그게 특정 패턴과 매치될 경우, 저장 큐에 들어갔던 메시지들은 모두 프로세스에 도착했던 순서대로 다시 메일박스로 들어간다. 

### 6. 등록된 프로세스 

* 관련 BIF
    * register(AnAtom) : 프로세스 Pid를 AnAtom이라는 이름으로 등록한다. 일종의 alias
    * unregister(AnAtom) : AnAtom과 연관된 모든 등록을 제거한다. 만일 등록된 프로세스가 중그면 자동으로 등록이 해지됨
    * whereis(AnAtom) -> Pid | undefined : AnAtom이 등록되었는지 조사. 있으면 해당 Pid를 반환, 없다면 undefined를 반환 
    * registered() -> [AnAtome::atom()] : 시스템에 있는 모든 등록된 프로세스들의 목록을 반환한다. 

* 관련 예제 코드 [Clock](./clock/clock.erl)


### 7. MFA로 띄우기

* 코드를 동적으로 업그레이드할 것이라면
* 다른 형태의 spawn을 사용해야 한다.
* spawn(Mod, FuncName, Args)
    * Args는 인수 리스트
    * 이 spawn으로 새로 생성된 프로세스는 Mod:FunName(Arg1, Arg2...)를 평가한다.
* Module, Func, Args의 앞글자를 따서 MFA라고 부른다. 
* 실행 중인 프로세스가 사용되는 동안 새 버전의 모듈 코드가 컴파일되면, 그 새 버전의 모듈 코드로 프로세스가 정확하게 업데이트되는 것을 보장하는 적합한 방법!
* 이건 동적 코드 로딩에서 따로 공부하도록


### 8. 과제

* spawn(Fun)을 AnAtom으로 등록하는 start(AnAtom, Fun) 함수를 작성하여라. [Code](./task/task1.erl)
* Ring 벤치마크를 작성, 링에 프로세스를 N개 생성하자, 링을 돌며 M번 메시지를 보내 총 N*M 개의 메시지를 보내자. N, M을 바꿔가며 시간 변화를 측정해봐라. [Code](./task/task2.erl)

