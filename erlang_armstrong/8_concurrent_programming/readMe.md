## Chapter 8. 병행 프로그래밍

### Intro

* 프로세스는 얼랭 함수를 평가할 수 있는 작도 독립적인 가상 기계
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
