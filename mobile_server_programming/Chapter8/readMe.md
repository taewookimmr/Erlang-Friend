## Chapter 8. 채팅과 메신저

### Intro

* 채팅 기능은, 단순하게 생각하면, 전달로 구성되어 있다.

* 중요한 것은 얼마나 많은 사용자가 동시에 채팅을 하느냐, 채팅 메시지는 어떻게 저장하고, 어떻게 전달할 것인지, 실시간 전용으로 할 것인지, 오프라인일 때도 볼 수 있어야 하는지, 보안은 어떻게 할 것인지, 이런 것을 다 고려해야 하는 상황이 오면 복잡스, 어렵스

* 얼랭 그 자체로도 메시지를 주고 받는, 채팅 중인 것들

* WhatsApp과 LOL의 채팅 서버는 얼랭으로 이루어져 있다!


### OTP application

* OTP는 얼랭에서 제공하는 여러 라이브러리의 집합

* gen_server 모듈

    * generic server 프로세스를 생성할 수 있다. 
    * 클라이언트-서버 구조로 동작하는 얼랭 프로세스를 생성할 때 사용한다.
    
    * gen_server:start_link, 
        * 이 함수를 실행하면 callback 함수인 Module:init이 실행된다. Module은 생성할 서버의 이름으로 대체해서 생각하면 된다.
        * spawn 함수를 이용해서 생성한 프로세스 : 일반 프로세스
        * gen_server로 생성하는 프로세스 : 특수 프로세스 -> spawn이 아닌, proc_lib:init_ack를 이용해서 프로세스를 생성한다. 
        * supervior에 의해 관리될 수 있는 프로세스가 특수 프로세스

    * gen_server:call, cast
        * callback은 Module:handle_call, cast
        * 차이점 : call은 메시지를 보냈을 때 응답값이 필요한 경우에  사용, cast는 응답값 받을 필요가 없을 때 
    
    * Module:handle_info : test_server ! {message} 형식으로 직접 메시지를 전송 받는 것에 대한 처리 

    * Module:code_change" 

