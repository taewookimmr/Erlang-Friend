## Chapter 4. 로그인

### API 설계

* 아이디, 패스워드 등의 인증 정보를 서버에 저장하는 작업이 선행되어야 한다. Sign In

* /login/id=myid%password=mypass
    
    * HTTP URL(Uniform Resource Locator)에 Query String으로 변수를 포함시키는 방법
    * 위와 같은 방법으로 원하는 값을 서버로 전달하는 데 있어서의 단점
        * 길이 제한이 존재한다. 
        * URL은 2048bytes 이내로 사용하는 것이 문제 발생을 방지할 수 있다.

    * 해결법은? Query String을 URL에 포함시켜서 전달하지 말고 body에 넣어서 전달 
    * HTTP  GET Method 말고 POST Method를 이용하면 된다.


* HTTP Method

    |Method|Description|
    |---|---|
    |OPTIONS|해당 메서드를 지원하는지 문의|
    |GET|URL에 해당하는 자료의 전송을 요청|
    |HEAD|URL에 해당하는 자료의 Head 정보만 요청|
    |POST|서버에 데이터나  메시지를 전송|
    |PUT|해당 URL에 자료를 저장한다|
    |DELETE|URL에 해당하는 자료를 삭제한다|
    |TRACE|요청한 메시지가 정상이면 클라이언트로 다시 전달|
    |CONNECT|Proxy에서 사용하도록 예약된 Method|
    

* API 정의

    |Method|API|Body Query String|Description|Retun Data|
    |---|---|---|---|---|
    |Post|/hello/world|...|서버와 테스트 통신| result::ANY|
    |Post|/join|id::아이디, password::패스워드|회원가입| result::ok|fail|
    |Post|/login|id::아이디, password::패스워드|로그인| result::ok|fail|

    * 위와 같은 표로 정리 : 클라이언트 개발자와의 소통 굿 

### 기능 구현 

* Cowboy router : 여러 개의 Path에 대해 동작하도록 작성해야 한다.

    ```erlang
    cowboy_router:compile([{HostMatch, [{PathMatch, Handler, Opts}]}]).

    % HostMatch : 모든 호스트에 대해 동작하도록 하려면 _을 넣어 주면됨
    % [{"/:api/[:what]", mon_http,[]}]
    % 이렇게 PathMatch, Handler, Opts를 설정하면, /A or /A/B 타입의 API에 한해서 Handler가 실행될 것이다
    % 바인딩 된다는 표현을 사용하네, A는 api라는 key에 바인딩, B는 what이라는 key에 바인딩된다.
    % [:what]으로 표현한 이유는 생략이 가능하도록 하기 위함 
    ```

* URL Parser 

    * URL parser는 /a/b/c 등의 URL 부분을 우리가 원하는 요청에 대해서 지정된 API를 실행하도록 분류하는 기능을 한다.
    * cowboy_req:binding(key, Req) 를 사용하여, router(mon_app.erl에 있는 cowboy_router:compile)에서 설정한 api, what, opt 값을 가져온다


### Dynamic Code Loading

* Hot Code Loading 이라고도 함
* 프로그램을 실행 중인 상태로 코드를 컴파일하고, 바로 프로그램에 적용할 수 있는 기능 
* erlang shell에서 : c("src/mon_http.erl", [{outdir, "ebin"}]). 이렇게 명령하면 이전에 수정했던 mon_http.erl가 컴파일된다. 이전 mon_http 모듈이 제거되고, 새로운 mon_http가 로딩된다. 

* ok 잘 되는 것 같다. 그런데 책과는 하나 다른게 있는데 출력이 한 줄 더 생긴다는 점
    * api=<<"favicon.ico">>, what=undefined, opt=undefined 
    * 여기서 favicon.ico는 뭐지 -> 파비콘은 웹페이지에 접속했을때, 상단 탭에 보여지는 아이콘을 일컫는다. 이 아이콘은 즐겨찾기에 웹페이지를 등록할때도 사용된다. 웹사이트를 대표하는 로고(logo)의 개념과 비슷하며, 사이트의 성격을 드러내기도 한다. [출처](https://webdir.tistory.com/337)

* Dynamic Code Loading을 잘 활용하면 서버를 업데이트 할 때에 서버를 종료하지 않고 운영중인 상태로 업데이트를 진행할 수 있다.

* c() 말고 l() 명령어는 : 컴파일된 beam파일 로딩해주는 역할을 한다. 서버에서 컴파일하기 싫을 때, 컴파일된 beam을 갖고 와서 업데이트 할 때 사용될 수 있는 함수이다. 

* Code Reloading Module을 만들어 보자.
    * 역할 : 현재 서버에 로딩된 모든 파일을 검사, 모듈이 변경되었는지 판단, code:all_loaded() 함수가 그 역할을 한다.

    ```erlang
    start()->
        Pid = spawn(mon_reloader, loop, [erlang:localtime()]),
        timer:send_interval(timer:seconds(1), Pid, check). % Pid에게 check 메시지를 1초 간격으로 보내겠다는 의미
    ```

    * 확실히 armstrong으로 공부하고 보니까 이해가 더 쉽게 된다 
    
    ```erlang
    check(From, To, Module, Filename)->
        case file:read_file_info(Filename) of
            {ok, #file_info{mtime=MTime}} when MTime >= From, MTime < To ->
                reload(Module);
            _ ->
                pass
        end.

    % when MTime >= From, MTime < To 이 부분을 믿어도 되는지 잘 모르겠다.
    % MTime > To 인 상황이 생길 수 있지 않은가? 아 그렇지 그러면 skip되겠지 한번은, 그리고나서 그 다음 사이클에선 잡히겠구나 ok     
    ```
    


### /login, /join

* cowboy_req:body_qs() : HTTP의 Body 값을 읽어 온다. 

    ```erlang
    ...
    %% Data loading
    {ok, Data, Req4} = cowboy_req:body_qs(Req3),
    ...

    handle(<<"login">>, _,_,Data) ->
    Id = proplists:get_value(<<"id">>, Data),
    Password = proplists:get_value(<<"password">>, Data),
    case {Id, Password} of
        {<<"testid">>, <<"testpass">>} ->
        <<"{\"result\":\"login ok\"}">>;
        _ ->
        <<"{\"result\":\"login fail\"}">>;
        end;

    ...
    ```

    * proplists:get_value(First, Second) : First는 key, Second는 Data
    * 쉽게 생각해서 key를 넣어주면 value를 뱉어내는 함수여


* 회원가입 기능, /join을 구현하려면,, 데이터를 어딘가에 저장해야할 필요가 있다. ETS, Dets ㄱㄱ

### ETS와 Dets

* Erlang Term Storage, 얼랭에 내장된 메모리 데이터베이스
* key-value 방식의 NoSQL, Erlang VM에 내장됨

* Dets : Disk ETS : ETS 기능을 파일 베이스로 만든 버전, 테이블 하나당 2GB 용량 제한 

* ETS의 모든 기능은 ets모듈에 BIF로 구현됨

* ETS에 데이터를 저장히기 위해서는 테이블 생성해야함.

    * 테이블에는 얼랭의 튜플 형식만 저장가능
    * 기본적으로 첫번째 값이 p-key

* ETS 테이블 타입 : set, ordered_set, bag, duplicate_bag

    * set : 오직 하나의 key에 대한 object만 허용. write, delete, select의 Time complexity = O(1)
    * set과 같지만 정렬된다. 성능은 O(log n)
    * bag : key에 대해서 중복 데이터가 가능하다. 그런데 전체적으로는 구별되어야 한다.
        {orange,1,500}, {orange,2,400}이 허용된다는 것
    * duplicate bag : {apple,1,2}, {apple,1,2}가 허용된다. 

* 테이블에 대한 Access 권한 : public, protected, private

    * public : 어떤 프로세스든지 rw가능
    * protected : 소유자만 rw, 나머지는 r, 이것이 default
    * private : only 소유자 rw

* 테이블 create, write, read 예시
    ```erlang
    1> ets:new(test_table, [public, named_table]).
    >> test_table
    2> ets:insert(test_table, {apple,1}).
    >> true
    3> ets:lookup(test_table, apple).
    >> [{apple, 1}]
    ```


### 중간 점검

* 데이터 베이스의 부재, 회원 정보 저장 문제
* User session이 없다 : 로그인 하면 무한정 로그인, 로그아웃 개념이 없다. 동시 로그인도 판별할 수단이 없다. 