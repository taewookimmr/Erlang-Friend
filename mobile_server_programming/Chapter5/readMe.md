## Chapter 5. Database

### TO be filled next time


### Schema와 얼랭 

* Mnesia를 사용하기 위해서는 먼저 Schema를 생성해야 한다. 관계형 DB에서의 Schema와는 다르다.

    * mnesia:create_schema(NodeList), Mnesia 데이터베이스를 초기화하는 과정

    * 인자값으로 전달하는 NodeList는 Mnesia를 초기화할 얼랭 노드들의 리스트

    * 얼랭 노드는 실행한 얼랭 런타임 시스템의 이름을 의미한다. node()는 현재 자신의 노드

    * -sname, -name 비교 : -sname은 물리적으로 하나의 서버에 있는 노드끼리의 통신이  가능해지고, -name은 네트워크를 통해서 노드들 간의 연결이 가능하게 된다.

    * epmd.exe는 Erlang Port Mapper Daemon으로 얼랭 노드들 간의 TCP/IP 통신을 담당하는 역할을 한다. 기본적으로 4369번 TPC port를 사용한다. epmd가 실행되고 있지 않으면 여러 개의 얼랭 노드를 이용한 분산 시스템을 만들 수 없다.

    * NodeList에 여러개의 노드를 적어주면 자동으로 데이터가 다른 노드로 복제되어, 분산 mnesia 서버를 운용하게 된다.

    * mnesia:create_schema()를 직접 실행한 얼랭의 노드가 Master 노드가 된다. 다른 노드를 Slave 노드라고 한다. mnesia가 동작 중일 때는 schema를 생성할 수 없다. (왜냐하며 schema 생성이 곧 mnesia를 초기화하는 의미이기 때문)


### 테이블 생성

* mnesia:create_table(Name, Args) 함수를 사용하여 테이블을 생성한다.

    * Name : 생성할 테이블 이름을 atom 형식으로 작성,
    * Args : 테이블 타입, 각종 설정값을 {Key, Value} 형식의 튜플들의 리스트로 입력한다.

* 3가지 종류의 Mnesia 테이블

    * ram_copies 
        
        RAM에 저장된다. 마치 ETS와 같이 서버가 종료되면 데이터는 사라진다. 하나의 테이블에 저장할 수 있는 최대 사이즈는 시스템의 메모리 사이즈와 같다.

    * disc_only_copies 

        데이터를 디스크에만 저장하고 싶을 때 사용. 사이즈 제한은 2GB(왜냐하면 Dets의 한계가 2GB이기 때문), 이 방식을 추천하지는 않네

    * disc_copies 
    
        데이터를 디스크, 메모리에 동시에 저장. 연산은 메모리에서 하여 속도 빠름, 백업은 디스크에 하여 안전. 가장 추천하는 옵션


* mnesia:create_table의 주요 설정값

    * {attributes, List} 

        * 테이블의 컬럼 이름을 리스트 형식으로 입력한다. : 관계형 디비의 스키마가 이 부분에 대응
        * -record(users, {id, password}) 라는 레코드가 있을 때 
        * {attributes, record_info(fields, users)}라고 작성하면 id, password 가 일종의 속성이 된다. 
        * record_info의 역할은 : 하나씩 컬럼 이름을 적어주는 것이 아니라 이미 정의된 record를 인수르 받아서 컴파일 할 때 자동으로 변환(하나하나의 컬럼으로)하여 입력된다. 

    * {ram_copies, NodeList}, {disc_only_copies, NodeList}, {disc_copies, NodeList}      

        * 분산 시스템을 운영할 경우 하나의 테이블을 여러 개의 노드에 다른 형식으로 저장할 수 있다. 어떤 테이블은 Master 노드에는 disc_copies로 저장하고, Slav 노드에는 ram_copies로 저장하는 등의 설정이 가능하다. 

    * {type, Type} 

        * ETS Table의 Type과 같다 : set, ordered_set, bag 중에 선택, default는 set

    * {index, Intlist}

        * 기본적으로 사용되는 record의 첫번째 값인 p-key를 제외하고, 다른 index를 추가하고 싶을 때 사용

    
    * example : mnesia:create_table(users, [{attributes, record_info(fields, users)}, {disc_copies, [node()]}])
        
        * 구조를 눈에 익히자 mnesia:create_table(users, [tuple1, tuple2, tuple3, ...])

* 쓰기 읽기

    * 생성한 테이블에 rw하는 것은 transaction을 사용하느냐 안하느냐 에 따라서 방법이 다르다.
    
        * 간단하게 rw하려면 dirty 함수를 사용, transaction이 적용되지 않는다는 것은 ACID가 완벽하지 않다는 의미

        * Transaction을 사용하는 방법은 다음과 같다.

            ```erlang
            Password = "test_password",
            F = fun() ->
                [U] = mnesia:read(users, "test_id"),
                U1 = U#users{password=Password}
                mnesia:write(U1)
             end,
            mnesia:transaction(F).
            ```
        * 트랜잭션이 실행되면서 locking, logging, replication, checkpoints, subscription, commit의 단계를 거침, 마지막에 commit까지 끝나야 디스크에 쓰는 것이 완료됨. 

        * 분산 디비에서, mnesia:transaction() 함수가 정상적으로 리턴하였다고 해서, 모든 분산 서버드이 데이터를 디스크에 썼다는 것을 의미하지는 않는다. 

    * mnesia:activity(AccessContext, Fun, [, Args]) -> ResultOfFun | exit(Reason)

        * mnesia:transaction(F) 대신에 mnesia:activity(transaction, F)를 사용할 수 있다.
        * mnesia:sync_transaction(F) 대신에 mnesia:activity(sync_transaction,F)를 사용해도 된다. 

### DB 연동 구현

* 쭈욱 구현해보고 정리하는 방향으로

* 직접 보는 것이 빠르겠다

    * [mon_db.erl](./monTester/src/mon_db.erl)
    * [mon_users.erl](./monTester/src/mon_users.erl)
    * [mon_http.erl](./monTester/src/mon_http.erl)




