## Chapter 11. 분산 컴퓨팅

### Intro

* 정식으로 대규모 서비스를 하려면 여러 대의 컴퓨터에 서버 프로그램을 설치해서 동작시켜야 한다.
* 분산 시스템은 간단하지 않다.
* 단순한 시스템이 아니라면 분산 컴퓨팅을 염두에 두고 해당 기능을 개발해야 한다. 

### 1. Nodes

* 실행중인 얼랭 런타임 시스템을 Node라고 한다.

* 여러 대의 컴퓨터에 각각의 노드를 실행할 수 있는 환경이라면 -sname 대신 -name을 사용하여 각각의 컴퓨터에서 실행 가능

* 분산 시스템 환경이 되려면 Node끼리 서로 연결되어 있어야 한다. 
    * Node들 사이에서 서로 같은 key를 공유해야 한다.
    * 이를 magic cookie라고 한다. 

    * 기본 cookie 값은 모두 같기 때문에 보안에 문제가 생길 수 있다. 그래서 특정한 cookie값을 설정해야만 한다. 

    * cookie를 설정하는 방법
        * erlang:set_cookie() 함수를 실행하는 방법
        * -setcookie를 사용해서 설정하는 방법 
            ```
            1> werl -sname a -setcookie good
            2> werl -sname b -setcookie good
            ```

### 2. RPC

* Remote Procedure Call Service : 자신의 노드가 아닌 연결된 원격지 노드의 함수를 호출할 수 있는 기능 

* call(Node, Module, Function, Args) -> Res | {badrpc, Reason}

* cast, yield, multicall 이런것은 rpc를 따로 공부하세요

### 3. Remote Processes

* remote process는 원격지 노드에서 동작하고 있는 프로세스들을 의미한다. 

* spawn(Node, Fun) -> pid() 

