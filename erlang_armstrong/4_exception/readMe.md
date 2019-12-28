## Chapter4 Exception

---
### 1. 예외 

* crashing 
* 예외는 내부 오류를 만나거나 혹은 코드에서 명시적으로 throw(Exception)이나 exit(Exception) 또는 erlang:error(Exception)을 호출하는 경우, 시스템에 의해 발생된다. 

* erlang에서 예외를 잡는 방법은 2가지
    * 예외를 발생시키는 함수를 try-catch로 감싸는 것
    * 함수 호출을 catch식으로 감싸는 것 

---
### 2. 예외 발생시키기

* 패턴 매칭 오류, no clauses in a function match
* 잘못된 형의 인수로 BIF를 호출하는 것
* 예외를 만들어내는 함수를 호출해, 명시적으로 오류를 생성할 수도 있다.
    
    * exit(Why) 
        * 현재 프로세스를 종료하고자 할 경우 사용 
        * 이 예외를 잡지 않으면 {'EXIT', Pid, Why} 메서지가 현재 프로세스와 연결된 모든 프로세스로 broadcasted 된다. 
    
    * throw(Why)
        * 호출자가 잡을지도 모르는 어떤 예외를 던지는 데 사용한다.
        
    * erlang:error(Why)
        * crashing error 를 표시하는 데 사용한다. 


---
### 3. try-catch

```erlang

try FuncOrExpressionSequence of
    Pattern1 [when Guard1] -> Expression1;
    Pattern2 [when Guard2] -> Expression2;
    ...
catch
    ExceptionType: ExPattern1 [when ExGuard1] -> ExExpression1;
    ...
after
    AfterExpressions
end 

```

### 4. try-catch 프로그래밍 관용법

```erlang
exgen(1) -> a;
exgen(2) -> throw(a);
exgen(3) -> exit(a);
exgen(4) -> {'EXIT', a};
exgen(5) -> erlang:error(a).

demo1()->
    [catcher(I) || I <- [1,2,3,4,5]].

catcher(N) ->
    try exgen(N) of
        Val -> {N, normal, Val}
    catch
        throw : X -> {N, caught, thrown, X};
        exit : X -> {N, caught, exited, X};
        error: X -> {N, caught, error, X}
    end.

```

### 5. catch

* catch 기본 명령(primitive)를 사용하는 것이다.

```erlang
demo2() ->
    [{I, (catch exgen(I))} || I <- [1,2,3,4,5]].
```

* 3, 4일 때 동일한 결과가 나온다. 하나는 정상이고 하나는 exit을 사용했는데,

### 6. 오류 메시지 개선하기

* erlang:error의 용도 중 하나는 오류 메시지의 품질을 개선하는 것
* 오류 메시지 부분을 개선한 래퍼를 작성할 수 있다.

```erlang
sqrt(X) when X < 0 ->
    erlang:error({squareRootNagativeArgument, X});
sqrt(X) ->
    math.sqrt(X).
```
```erlang
1> lib_misc:sqrt(-1).
>> ** exited : blah blah blah
```

### 7. 가능한 모든 예외 잡기

```erlang
try Expr
catch 
    _:_ -> ... 모든 예외를 처리하는 코드 ...
end
```


### 8. stack tracing

* erlang:get_stacktrace()를 호출하여 가장 최근의 스택 추적을 찾을 수 있다.