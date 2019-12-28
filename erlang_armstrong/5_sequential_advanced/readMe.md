## Chapter 5 Advanced Seq. Programming

## 이 부분은 사전식으로 공부하도록 합시다. 아직은 잘 모르겠네 191229

---
### 1. BIF 

* Built-In Function
* 이 함수들은 얼랭으로 프로그래밍 할 수 없는 작업들을 수행한다.
    * list to tuple
    * present time and date 



### 2. Binary

* 리스트나 튜플보다 훨씬 효율적인 방식으로 데이터 저장
* 바이너리에서 정수를 사용할 경우, 0에서 255 범위 내에 있어야 한다 
```
바이너리 <<"cat">> 은 <<99,97,116>>의 축약형이다. 
```
* 바이너리 조작 BIF

    * @spec list_to_binary(IoList) -> binary()
    * @spec split_binary(Bin, Pos) -> {Bin1, Bin2}
    * @spec term_to_binary(Term) -> Bin
    * @spec binary_to_term(Bin) -> Term
    * @spec size(Bin) -> Int


### 3. 비트 구문

* 16비트 색상 묶고 풀기


```erlang
1> Red=2.
2> Green=61.
3> Blue=20.
4> Mem = <<Red:5, Green:6, Blue:5>>.
>> <<23, 180>>
```

* 라인4에서 16비트 크기를 담는 2바이트 바이너리를 생성함
* 다음과 같이 워드를 풀어낸다

```erlang
5> <<R1:5, G1:6, B1:5>> = Mem.
```

* 비트 구문식
    * to be continued
