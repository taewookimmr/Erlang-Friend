## Chapter 6. User session

### 1. 유저 세션 

* 세션 : 유저가 접속한 이후에 로그이된 상태를 지속적으로 관리하기 위해 필요

* 모바일에서는 로그인과 다르게 로그아웃은 서버에서 자동으로 관리해 주어야 한다.

* 로그인에 성공한 유저의 세션을 만들고, 해당 유저가 특정 시간 동안 아무런 행동을 하지 않는다면 자동으로 세션을 종료해서 로그아웃 되도록 하는 기능이 필요하다.

* PC 기반 애플리케이션 서비스 : TCP 소켓이 접속되고 끊어지는 것을 세션의 만료라고 볼 수 있다.
    
    * 그런데, 모바일, 웹 기반에서는 TCP 소켓의 connecton으로 세션을 분류하는 것은 비효율적
    * 사용자 행동과 서비스에 맞는 적절한 시간을 이용해서 세션의 만료를 판단해야 함.

* 얼랭 프로세스를 이용해서 유저의 세션을 만들 것이다.

### 2. 얼랭 프로세스

* 얼랭에서 프로세스 : OS의 프로세스를 생각해서는 안된다. 
* 얼랭에서 이루어지는 모든 연산은 프로세스 안에서 이루어짐, 프로세스 밖에서 실행되는 것은 없다.

* self().를 입력하면 현 프로세스의 Pid 값을 확인할 수 있다. ex) <0.86.0>

    * 구조 <A.B.C> 
        * A : 얼랭 node를 구분하는 고유한 번호,로컬 프로세스의 경우, 0. 
        * B : 프로세스의 인덱스 부분이고, 프로세스 인덱스 번호가 최대값을 넘어서면 C 부분의 숫자가 하나씩 증가

* 현재 얼랭 시스템에서 동작 중인 모든 프로세스 : i().로 확인 

    * Registered 프로세스, register(Name, Pid)를 이용해 이름을 할당할 수 있다. 특정 이름의 프로세스의 PID를 알고 싶다면 whereis(Name) 함수를 사용

* 특정 프로세스의 정보 자세히 : erlang:process_info(Pid) 사용 
* 얼랭 시스템이 떠 있는 모든 프로세스의 개수 : erlang:system_info(process_count)
* 시스템의 최대 프로세스 개수 설정 : erlang:system_info(process_limit)  [armstrong 참고하도록](https://github.com/taewookimmr/Erlang-Friend/tree/v1/erlang_armstrong/8_concurrent_programming)



### 3. 얼랭 프로세스 내부 구조

* 얼랭 프로세스 

    * 얼랭 VM 내부에 존재하는 메모리 영역일 뿐
    * 크게 4가지로 구분되는 메모리 영역을 가지고 있다.
        * PCB(Process Control Block)
        * Stack
        * Heap(프로세스 내부에서 큰 데이터를 다룬다면 더 커지게 됨, GC가 프로세스마다 동작하여 조절)
        * Mailbox(Message Queue)
    
    * java JVM과의 차이점
        * java JVM이 GC를 동작해서 쓰레기를 수집하는 순간에 JVM 시스템 전체가 정지(stop-the-world)
        * erlang GC는 전체 시스템을 정지하지 않음. 오직 하나의 프로세스만 아주 잠깐 정리 

    * 프로세스 내부는 4가지 메모리 영역 관리, 실행을 위한 pointer들도 가지고 있다.
        * instruction pointer
        * stack pointer
        * heap top
        * reduction counter
        * X registor
    
* 얼랭 프로세스 스케줄러 

    * 얼랭이 어떻게 멀티 프로세스 환경에서 적합하도록 모든 CPU의 core를 사용하는 지 알기 위해서는 스케줄러를 이해해야 함
    * 얼랭의 process scheduler는 논리적 CPU당 하나씩 쓰레드로 생성되는 것이 기본 설정? 아직 감은 안오네
    * 일단 쭈욱 따라치며 용어에 익숙해지자.
    * 논리적 CPU는 가상 CPU라고도 불린다. 운영체제가 인식하는 CPU를 의미
    * 운영체제에서 CPU의 hardware thread 등으로 생겨난 CPU instance들도 하나의 독립적인 CPU로 인식한다.
        * 여기서 hardware thread : 인텔의 hyper-threading 기술처럼 하나의 core 안에 병렬로 동작하는 멀티 쓰레드를 의미. 운영체제는 각각의 hardware thread를 독립적인 cpu로 인식

    * Erlang/OTP 22 [erts-10.5] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1]
        * 얼랭이 실행될 때마다 뜨는 메시지
        * [smp:4:4] : 스케줄러 쓰레드 : 논리적 CPU의 개수가 표시된것 
        * [async-thread:1] : file I/O 전용으로 생성되는 thread pool
    
    * 스케줄러는 각각의 Run Queue를 가지고 있음, Run Queue에는 할당 받은 프로세스들이 있다. 
    * 선점형 스케줄러에 해당한다. 

    * 얼랭 VM의 scheduler들은 각각의 Run Queue에 프로세스들을 실행하는데, 만약 한쪽 scheduler가 일을 다했는데, 다른 녀석은 바쁘다면, 바쁜 쪽의 일을 훔쳐와서, 효율성을 높인다.
        * 프로세스가 다른 scheduler로 새롭게 할당되는 것을 context switch인데 얼랭 VM에서는 현재 process struct를 가리키는 pointer를 변경하는 수준으로 간단하게 처리가 가능하다.

        




