## Chapter 7. 병행성

### 1. Intro 

* 병행성
* 세상은 병렬이다. 그렇지
* 우리 인간에겐 공유 메모리가 없다. 내 뇌는 나만 사용한다.
* 우리는 대화로 정보를 공유한다.
* 얼랭도 마찬가지다 메시지를 주고 받으며 진행한다.
* 얼랭에는 lock이 없다

* 얼랭 프로그램은 많은 프로세스로 구성, 프로세스간 메시지를 보낼 수 있다.
* 이렇게 받은 메시지는 이해될 수도 있고 아닐 수도 있다. 어떤 메시지가 수신되어 이해되었는지 알려면, 그 프로세스로 메시지를 보내고 응답을 기다려야 한다.
* 프로세스들은 서로 쌍으로 연결될 수 있다. 
* spawn, send(!), receive라는 primitive 명령어로 병행 프로그램을 작성할 수 있다. 