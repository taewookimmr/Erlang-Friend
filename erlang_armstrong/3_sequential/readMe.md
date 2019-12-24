## 순차 프로그래밍

### 3.1 Module

1. 모듈은 erl엣 코드의 기본 단위
2. 모듈을 컴파일한다. 결과물은 .beam 확장자를 갖는다. 컴파일시 erlc 명령어 사용
3. Beam : Bogdan's Erlang Abstract Machine

    geometry.erl
    ```erlang
    -module(geometry).
    -export([area/1]).
    area({rectangle, Width, Ht}) -> Width * Ht;
    area({circle R}) -> 3.14159 * R * R.
    ```
    1) area 함수는 두 절(clause)로 구성된다.
    2) 절은 ;으로 구분된다. 마지막절은 .(dot-whitespace)으로 끝낸다.
    
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
