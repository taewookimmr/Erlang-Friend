## Chapter 17. Mnesia 얼랭 데이터베이스 

[소스코드](./test/test_mnesia.erl) 보면서 공부합니다.

### 1. 테이블에서 모든 데이터 선택하기

```erlang
demo(select_shop) ->
    do( qlc:q ([X || X <- mnesia:table(shop)]));
```
* qlc:q(Q)에서 Q는 마치 list comprehension가 유사하다. query list comprehension이라고 부르네
* Mnesia 데이터베이스에서 데이터를 엑세스하는 데 사용할 수 있는 모듈 가운데 하나이다.

* qlc:q/1의 인수는 리스트 해석 리터럴이어야 함. 그 자체여야 한다는 것이다.
```erlang
%% 이 코드는 위 코드와 동일하지 않다.
Var = [X || X <-mnesia:table(shop)],
qlc:q(Var)
```

### 2. 테이블에서 데이터 추출하기 

```erlang
demo(select_some) -> 
    do( qlc:q( [{X#shop.item, X#shop.quantity} || X <- mnesia:table(shop)]));
```
* X는 shop형 레코드이다. 
* 레코드 문법을 떠올려 보자. X#shop.item은 shop 레코드의 item 필드를 가리킨다.


### 3. 테이블에서 조건적으로 데이터 선택하기

```erlang
demo(reorder)->
    do(qlc:q([X#shop.item || X <- mnesia:table(shop), 
                                X#shop.quantity < 250]));
```

### 4. 두 테이블로부터 데이터 선택하기 

```erlang
1> demo(join) ->
2>    do(qlc:q([X#shop.item || X <- mnesia:table(shop), 
3>                                X#shop.quantity < 250,
4>                                Y <- mnesia:table(cost),
5>                                X#shop.item =:= Y#cost.name, %% join의 실체
6>                                Y#cost.price <2])).                                
```
* 위 코드에서 line5와 line6의 위치를 바꿔도 의도대로 작동하는가? 알아보자
    * 동일하게 작동한다. demo(join_test)를 실행해보면 알 수 있다.


### 5. DB에 데이터 추가하고 제거하기

* 만약 새로 생성하려는 레코드가 디비 테이블의 기존 행과 동일한 p-key를 갖고 있다면, 그 행을 덮어 쓸 것이다. 그렇지 않으면 새로운 행이 만들어질 것이다.

### 6. 트랜잭션

```erlang

do_something(...) ->
    F = fun() ->
        ...
        mnesia:write(Row)
        ...
        mnesia:delete(Oid)
        ...
        qlc:e(Q)
    end,
    mnesia:transaction(F)
```

* F는 인수가 없는 펀이다. 왜 이렇게 구성할까?
    * 동일한 데이터에 동시에 접근하는 두 개의 프로세스를 상정해보자.
    * 두 프로세스가 모두 잔금을 모두 인출하려는 트랜잭션을 시도할 때, 바람직한 결과는 하나는 성공, 나머지 하나는 실패가 되어야 한다.
    * 이를 보장해주는 것이 바로 mnesia:transaction/1이다.
    * 이를 위해 Mnesia는 pessimistic locking 전략을 취한다. 
        * Mnesia transaction manager는 테이블에 접근할 때마다 맥락?에 따라 특정 레코드 또는 전체 테이블에 대해 잠금을 시도한다. 만약 그 결과로 deadlock이 발생할지도 모른다는 것이 감지되면(내부 알고리즘이 있나보네), 관리자는 즉각 트랜잭션을 중단하고 지금까지 했던 모든 변경을 취소한다.

        * 만약 어떤 다른 프로세스가 데이터를 엑세스하는 중이라서 transaction이 처음부터 실패하면, 시스템은 잠시 대기한 후,  다시 트랜잭션을 시도한다. 이런 이유로 트랜잭션 펀 안의 코드는 여러 번 계산될 수도 있다.

        * 펀안에서 출력을 하게 되면 여러 번 출력되는 것을 볼지도 모른다. 


### 7. 테스트 데이터 로드하기 

```erlang
example_tables()->
    [%% Ths shop table
        {shop, apple, 20, 2.3},
        {shop, orange, 100, 3.8},
        {shop, pear, 200, 3.6},
        {shop, banana, 420, 4.5},
        {shop, potato, 2456, 1.2},
    %% The cost table   
        {cost, apple, 1.5},
        {cost, oragne, 2.4},
        {cost, pear, 2.2},
        {cost, banana, 1.5},
        {cost, potato, 0.6}    
    ].
```
* 위 함수는 디비 테이블을 초기화할 데이터를 제공하는 함수

```erlang
reset_tables() ->
    mnesia:clear_table(shop),
    mnesia:clear_table(cost),
    F = fun() -> 
            foreach(fun mnesia:write/1, example_tables())
        end,
    mnesia:transaction(F).
```
* reset_tables() 함수안에 있는 F를 보자, foreach문의 구조도 다시 복습하고. 


### 8. do() 함수

```erlang
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
```

* 위에서 Q는 컴파일된 QLC 질의, qlc:e(Q)는 그 질의를 평가하고 질의에 대한 모든 답을 리스트로 반환한다. 

* qlc:e(Q)와 qlc:q(Q)의 차이는 뭔가요?