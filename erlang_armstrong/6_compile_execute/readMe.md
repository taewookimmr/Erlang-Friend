## Chapter 6. 컴파일, 실행 관련

### Intro

* erlang:halt()로 시스템을 중단할 수 있다. 이 방식으로 중단하면 약간의 불이익 따름
    * 다음에 다시 시작할 때 오류 복구 절차를 거쳐야만 한다.

* q() 명령을 사용하여 통제된 방식으로 중단하는 방법
    * 모든 열린 파일을 닫고, db를 멈추며, 모든 otp 애플리케이션을 순서대로 닫는다.
    * q()는 init:stop()의 별칭


### 1. 개발 환경 수정 
    
* 코드 로딩 검색 경로 설정 
    * 얼랭 런타임 시스템은 코드 자동 로드 메커니즘을 사용
    * 코드 로딩은 on demand 방식으로 수행된다.
    * code:get_path() : 현재 로드 경로의 값을 볼 수 있다.
    * @spec code:add_patha(Dir) => true | {error, bad_directory}
        새 디렉터리 Dir을 로드 경로의 앞에 추가
    * @spec code:add_pathz(Dir) => true | {error, bad_directory}
        새 디렉터리 Dir을 로드 경로의 끝에 추가


### 2. 프로그램을 실행하는 다른 방법

* erlang shell에서 컴파일 하고 실행하기
    ```erlang
    $ erl
    1> c(hello).
    2> hello:start().
    ```
* 명령 프롬프트에서 컴파일, 실행
    ```erlang
    $ erlc hello.erl
    $ erl -noshell -s hello start -s init stop 
    ```
    * -noshell : 인터렉티브 셸 없이 얼랭을 시작한다
    * -s hello start : hello:start() 함수를 실행한다.
        * 단, hello.beam이 이미 생성되어 있어야 한다. 즉 컴파일 되어 있어야 한다
    * -s init stop : apply(hello, start, [])가 완료되면 시스템은 init:stop()을 수행한다. 

    * shellscript를 만들어서 사용할 수 있다. 
        * window용으로 hello.bat
        * bash용으로 hello.sh
    
* Escript로 실행하기 
