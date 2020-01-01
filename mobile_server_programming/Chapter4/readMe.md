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
    