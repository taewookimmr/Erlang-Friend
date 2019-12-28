## 순차 프로그래밍

12. 간단한 리스트 처리 

* 리스트에 있는 요소들의 합을 계산하는 함수를 직접 작성해보자
    ```erlang
    sum([H|T]) -> H + sum(T);
    sum([]) -> 0.
    ```

* map 함수를 직접 작성해보자
    ```erlang
    % mylists.erl
    map(_, []) -> [];
    map(F, [H|T]) -> [F(H)|map(F,T)].
    ```    
    ```
    1> L = [1,2,3,4,5]
    2> mylists:map(fun(X) -> 2*X, L).
    >> [2,4,6,8,10]
    3> mylists:map(fun(X) -> X*X, L).
    >> [1,4,9,16,25]
    ```
* lists 모듈에 있는 map과 sum 함수를 사용해보자 
    ```erlang
    -module(shop2).
    -export([total/1]).
    -import(lists, [map/2, sum/1]).

    total(L) -> 
        sum(map(fun({What, N})->1*N end, L)).
    ```

13. 리스트 해석 

* list comprehension : 펀이나 맵, 필터를 사용하지 않고 리스트를 생성하는 식 
    ```erlang
    % example
    1> L = [1,2,3,4,5]
    2> [2*X || X <- L].
    >> [2,4,6,8,10]
    ```

    ```erlang
    % 이런것도 가능하다
    1> Buy = [{orange,4},{newspaper,1}, {apples, 10}].
    2> [{Name, 2*Number} || {Name, Number} <- Buy].
    >> [{orange,8},{newspaper,2}, {apples, 20}].
    ```

    * 익숙해지자, {Name, Number} 이건 튜플이다.

    ```erlang
    % 간단해진 total 함수, map 함수
    total(L)->lists:sum([shop:cost(A)*B || {A,B} <- L]).
    map(F, L)->[F(X) || X <- L]
    ```

    * 가장 일반적인 형태의 리스트 해석은 다음과 같은 형태 
        
        * [X || Qualifier1, Qualifier2, ...]
        * X는 임의의 식, 각 Qualifier는 생성기 혹은 필터에 해당한다.
        * 생성기는 Pattern <- ListExpr로 작성된다. ListExpr는 텀의 리스트로 평가되는 식이어야 한다. 
        * 필터는 술어(boolean을 반환하는 함수)이거나 불리언 식이다.
        * 리스트 해석의 생성기 부분은 필터 처럼 작동한다? 다음을 보세요

        ```erlang
        1> [X || {a, X} <- [{a,1},{b,2}, {c,3}, {a,4}, hello, "wow"]].
        >> [1,4]
        ```

14. 퀵 정렬 

* 리스트 해석을 두 개 사용하여 정렬 알고리즘을 작성

```erlang
qsort([]) -> [];
qsort([Pivot|T]) ->
    qsort([X || X <- T, X < Pivot])
    ++ [Pivot] ++
    qsort([X || X <- T, X >= Pivot]).
```

```
1> L = [23,6,2,9,27,400,78,45,61,82,14].
2> lib_misc:qsort(L).
>> 정렬된 리스트가 나온다 
```

* 말로 정리하면 Pivot을 중심으로 작은것은 왼쪽으로 일단 보내고, 큰것은 오른쪽으로 보낸다.
* 그리고 나서 왼쪽부, 오른쪽부에 대해서 각각 재귀적으로 정렬을 수행한다. 끝


15. 피타고라스 삼각형 

* pythag(N)은 AA + BB = CC를 만족하고, 변의 합이 N보다 작거나 같은 모든 정수 {A,B,C}의 리스트를 생성한다.

```erlang
pythag(N) ->
    [{A,B,C} || 
        A<- lists:seq(1,N),
        B<- lists:seq(1,N),
        C<- lists:seq(1,N),
        A+B+C =< N,
        A*A+B+B =:= C*C
    ].
```
```erlang
1> lib_misc:pythag(16).
>> [{3,4,5}, {4,3,5}]
```

16. 철자 바꾸기

* 오 permutation을 다루는 함수인가

```erlang
perms([])->[[]];
perms(L) ->[[H|T] || H <- L, T <- perms(L--[H])].
```

17. 가드 

* 가드는 패턴 매칭의 능력을 증가시키는 데 사용되는 구조
* 간단한 테스트와 비교를 수해할 수 있다. 
* 가드는 함수 정의의 헤더에서 사용할 수 있다. when 키워드를 사용한다.

```erlang
max(X,Y) when X>Y -> X;
max(X,Y) -> Y/
```

* 가드 시퀀스
    * G1;G2;G3...;Gn : 어느 하나가 참이면 true
    * G1,G2,G3...,Gn : 모두 참이어야 true



18. 레코드

* 레코드는 튜플 안의 특정 요소와 이름을 연관 짓는 방법을 제공 

```erlang
-record(Name, {
    key1=Default1,
    key2=Default2,
    key3,
    key4,
    ...
}).
```

* 단 -record 선언은 얼랭 소스코드 모듈에서만 사용 가능, shell에서는 사용할 수 없다.

* Name은 레코드의 이름, key1, key2 등은 레코드의 필드 이름, 언제나 에텀이어야 한다. 그리고 필드에 기본값을 지정할 수 있다. 

* 레코드를 정의?한 파일의 확장자를 .hrl로 하여 여러 모듈에서 인클루드하여 사용할 수 있다.

```erlang
% records.hrl
-record(todo, {status=reinder, who=joe, text}).
```

* 레코드가 정의되면 레코드의 인스턴스를 생성할 수 있다.
* shell에서 불러올 떄는 rr(read records)를 사용한다

* 레코드를 생성하고 갱신하기

    ```erlang
    1> rr("records.hrl").
    >> [todo]
    2> X = #todo{}.
    >> #todo{status = remainder, who = joe, text = undefined}
    3> X1 = #todo{status = urgent, text="Fix errate in book"}
    >> #todo{status = urgent, who = joe, text = "Fix errate in book"}
    4> X2=X1#todo{status=done}.
    ```
    * 라인 4의 문법의 의미 : 기존 레코드를 복사. 그리고 status 변경 


* 레코드의 필드 추출하기
    
    * 다른 것과 마찬가지로 페턴 매칭을 사용한다.
    ```erlang
    5> #todo{who=W, text=Txt} = X2.
    6> W.
    >> joe
    7> Txt.
    >> "Fix errate in book"
    ```

* 레코드는 변장한 튜플이다

    * rf 명령어를 통해 todo에 대한 정의문을 잊으라고 명령하고
    * 해당 객체를 출력해보면 튜플이 출력된다.
    * 내부적으로는 오직 튜플만 있을 뿐이다. 
    * 레코드는 튜플 속의 상이한 요소에 이름을 붙일 수 있게 해주는 편의 구문이다.

19. case와 if 식 

* case 식 

```erlang
case Expression of 
    Pattern1 [when Guard1] -> Expr_seq1;
    Pattern2 [when Guard2] -> Expr_seq2;
    ...
end
```
* 먼저 Expression이 평가된다. 예컨데 Value라고 평가 되었다고 하자.
* Value는 Patternx등과 매치가 일어날 때까지 차례로 매치한다.
* 매치가 되면 대응하는 식 시퀀스가 평가되고, 그 식 시퀀스를 평가한 결과는 case 식의 값이 된다. 
* 아무 패턴과도 매치하지 않으면 예외가 발생한다. 

* case 문을 이용하여 filter 함수를 작성해 보도록 

    ```erlang
    filter(P, [H|T]) -> 
        case p(H) of 
            true -> [H|filter(P, T)];
            false -> filter(P, T)
        end;
    filter(P, []) -> []. 
    ```

* if 식 

```erlang
if 
    Guard1 ->
        Expr_seq1;
    Guard2 ->
        Expr_seq2;
    ...
end 
```

* Guard1이 우선 평가된다. 이게 true로 평가되면 if의 값은 식 시퀀스 Expr_seq1의 평가로 얻어진 값이 된다. 
* false로 평가되면 다음으로 넘어가고 같은 절차를 밟는다. 끝까지 true인 것이 안나온다? 그러면 오류가 발생할 것이다.


20. 누산기

* 함수로부터 리스트를 두 개 얻으려면 어떻게 해야 할까?

```erlang
%example1
odds_and_evens(L) ->
    Odds = [X || X <- L, (X rem 2) =:= 1],
    Evens = [X || X <- L, (X rem 2) =:= 0],
    {Odds, Evens}.
```

```
1> example1:odds_and_evens([1,2,3,4,5,6]).
>> {[1,3,5], [2,4,6]}
```

* 이 코드는 리스트를 두 번 돈다는 것

```erlang
%example2
odds_and_evens_acc(L) -> 
    odds_and_evens_acc(L, [], []).

odds_and_evens_acc([H|T], Odds, Evens) ->
    case (H rem 2) of
        1 -> odds_and_evens_acc(T, [H|Odds], Evens);
        0 -> odds_and_evens_acc(T, Odds, [H|Evens])
    end;
odds_and_evens_acc([], Odds, Evens) -> 
    {lists:reverse(Odds), lists:reverse(Evens)}.
```

