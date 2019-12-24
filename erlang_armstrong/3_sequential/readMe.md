## 순차 프로그래밍

### 3.1 Module

1. 모듈은 erl에서 코드의 기본 단위
2. 모듈을 컴파일한다. 결과물은 .beam 확장자를 갖는다. 컴파일시 erlc 명령어 사용
3. Beam : Bogdan's Erlang Abstract Machine

    geometry.erl
    ```erlang
    -module(geometry).
    -export([area/1]).
    area({rectangle, Width, Ht}) -> Width * Ht;
    area({circle R}) -> 3.14159 * R * R.
    ```
    <1> area 함수는 두 절(clause)로 구성된다.
    <2> 절은 ;으로 구분된다. 마지막절은 .(dot-whitespace)으로 끝낸다.
    
    실행 예, erl cli를 사용한다
    ```
    우선 <<은 입력을 나타내고, >>은 출력을 나타낸다.

    << c(geometry).
    >> {ok, geometry} 
    << geometry:area({rectangle, 10, 5}).
    >> 50
    << geometry:area({circle, 1.4}).
    >> 6.16752
    ```
4. 위의 area 함수를 생각해보면, java의 관점으로 보면 over-loading과 관련되어 있다.
    * parameter의 개수가 달라서 구분이 된다고 생각할 수 있다.
    * erl의 관점에서는 어떻게 말로써 표현하는가? 패턴이 매치되는 함수가 실행된다. 매치되는 패턴이 없다면 런타임 오류와 함께 실패한다.

5. c, java로 작성한 코드와 비교해보자

    c언어
    ```c
    enum ShapeType {Rectangle, Circle, Square};
    struct Shape {
        enum ShapeType kind;
        union{
            struct {int width, height;} rectangleData;
            struct {int radius;} circleData;
            struct {int side;} squareData;
         } shapeData;
    };
    double area(struct Shape* s){
        if(s->kind == Rectangle){
            int width, ht;
            width = s->shapeData.rectangleData.width;
            ht = s->shapeData.rectangleData.ht;
            return width * ht;
        }else if (s -> kind == Circle){
            ...
        }
        ...
    }

    // c 코드에서는 패턴 매칭 연산을 함수의 인수에서 수행한다.  패턴 매치 코드를 작성(if-else)하고 그게 정확한지 확인하는 일은 오로지 프로그래머의 몫이다. 

    // 반면, 얼랭에서는 작성자가 패턴만 작성할 뿐이며, 프로그램에서 정확한 진입법을 선택하는 최적의 패턴 매칭 코드를 생성하는 일은 얼랭 컴파일러의 몫
    ```

    java
    ```java
    abstract class Shape{
        abastract double area();
    }
    class Circle extends Shape{
        final double radius;
        Circle(double radius){this.radius = radius;}
        double area(){return Math.PI * radius*radius;}
    }

    class Rectangle extends Shape {
        final double ht;
        final double width;
        Rectangle(double width, double height){
            this.ht = height;
            this.width = width;
        }
        double area(){return width*ht;}
    }
    ...

    // 확실히 erlang이 깔끔하구나.

    ```
6. Erlang에서의 재귀문을 살짝 맛보도록 하자

    ```erlang
    [{oranges, 4}, {newspaper,1}, {apple,10},{pears,6}, {milk,3}]
    ```
    이러한 쇼핑 목록(항목, 개수)이 있을 때, 쇼핑 비용의 총합을 구하는 프로그램을 작성해보자
    일단 개수당 가격은 1이라고 정하자. 

    ```erlang
    -module(shop).
    -export([total/1]).

    total([{What, N}|T]) -> N * 1+ total(T);
    total([]) -> 0.
    ```

7. 이름은 같고 애리티가 다른 함수, overloading에 대응되는 부분 

    * 함수의 애리티(arity)는 그 함수가 가지는 인수의 수
    * 한 모듈에서 이름은 같고 애리티가 다른 함수 두 개는 전적으로 다른 함수를 의미.
    
8. fun 

    * fun은 익명 함수이다. 
    ```erlang
    1> Double = fun(X) -> 2*X end.
    ```

    * temperature unit conversion code 이다
    ```erlang
    1> TempConvert = fun({c, C}) -> {f, 32 + C*18/10};
                ({f, F}) -> {c, (F-32)*10/18}
                end.
    2> TempConvert({c, 100}).
    >> {f, 212.000}
    3> TempConvert({f, 212}).
    >> {c, 100.000}
    ```




