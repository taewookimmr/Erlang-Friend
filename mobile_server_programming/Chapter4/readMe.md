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




