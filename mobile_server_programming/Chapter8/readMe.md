## Chapter 8. 채팅과 메신저

### Intro

* 채팅 기능은, 단순하게 생각하면, 전달로 구성되어 있다.

* 중요한 것은 얼마나 많은 사용자가 동시에 채팅을 하느냐, 채팅 메시지는 어떻게 저장하고, 어떻게 전달할 것인지, 실시간 전용으로 할 것인지, 오프라인일 때도 볼 수 있어야 하는지, 보안은 어떻게 할 것인지, 이런 것을 다 고려해야 하는 상황이 오면 복잡스, 어렵스

* 얼랭 그 자체로도 메시지를 주고 받는, 채팅 중인 것들

* WhatsApp과 LOL의 채팅 서버는 얼랭으로 이루어져 있다!


### OTP application

* OTP는 얼랭에서 제공하는 여러 라이브러리의 집합

* gen_server 모듈

* gen_fsm 모듈 

    * 범용 유한 상태 기계 : generic finite state machine을 만들 수 있다.

    * 센서에 의해 물체가 가까이 다가온 것을 감지하면 돌아가는 회전문이나 에스컬레이터도 간단한 유한 상태 기계
    * 게임에서 유저가 다가가면 갑자기 난폭하게 달려드는 몬스터의 행동페턴도 유한 상태 기계로 구현된 것.


* gen_event 모듈

    * 범용 이벤트 핸들러를 만들 때 사용, 이벤트 핸들러는 시스템의 에러나 알림 메시지 등의 로그를 처리할 때 유용하다.
    

### Supervisor

* 여러 개의 프로세스들을 관리하는 프로세스를 구현할 때 사용한다.
* OTP에서 제공하는 gen_server, gen_fsm, gen_event를 사용하는 이유 중 하나는 supervisor의 자식 프로세스로 등록해서 관리할 수 있기 때문.

* supervisor는 자식 프로세스들을 시작시키고, 동작 상태를 모니터링하게 된다. 만약 자식 프로세스가 오류 등으로 인해서 종료된다면 이를 재시작시키는 역할을 한다.

    * one_for_one : 어떤 한 자식 프로세스가 죽었을 경우, 그 녀석만 재시작
    * one_for_all : 하나 죽으면, 다 죽이고, 모두 다시 재시작 : 자식 프로세스들을 동시에 종료하고, 동시에 시작해야할 때 사용
    * rest_for_one : 죽은 프로세스보다 처음에 늦게 시작된 녀석들만 종료시키고, 이미 종료된 프로세스와 종료시킨 전체 프로세스를 재시작, 시작 순서에 어떤 의존성이 있을 경우에 사용한다! 그렇지 이런 경우 있지
    * simple_one_for_one : 한 종류의 자식 프로세스만 동적으로 추가할 때에 사용한다. 다만 supervisor가 이 녀석을 종료시킬 수 없고, 이 녀석 스스로 종료해야 한다.

* 자식을 실제로 등록하려면 {Id, StartFunc, Restart, Shutdown, Type, Modules}로 이루어진 child_spec()을 등록해야한다. 

    * Id : 주로 모듈 이름 사용
    * StartFunc : 자식 프로세스가 시작될 때 실행하게 될 함수, {M,F,A} 형시을 따름
    * Restart : 자식 프로세스가 종료되었을 경우, 어떻게 동작할지 설정한 값, Permanent, transient, temporary 이 중에 하나 선택

    * Shutdown : brutal_kill , 0 이상의 숫자나 infinity로 설정할 수 있는데, 자식 프로세스를 어떻게 죽일지를 정하는 값.

    * Type : worker 혹은 supervisor로 설정할 수 있다. 


### 구현하기 

* ok thank you