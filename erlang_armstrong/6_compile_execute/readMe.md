## Chapter 6. 컴파일, 실행 관련

## Intro

* erlang:halt()로 시스템을 중단할 수 있다. 이 방식으로 중단하면 약간의 불이익 따름
    * 다음에 다시 시작할 때 오류 복구 절차를 거쳐야만 한다.

* q() 명령을 사용하여 통제된 방식으로 중단하는 방법
    * 모든 열린 파일을 닫고, db를 멈추며, 모든 otp 애플리케이션을 순서대로 닫는다.
    * q()는 init:stop()의 별칭


## 1. 개발 환경 수정 
    
* 코드 로딩 검색 경로 설정 
    * 얼랭 런타임 시스템은 코드 자동 로드 메커니즘을 사용
    * 코드 로딩은 on demand 방식으로 수행된다.
    * code:get_path() : 현재 로드 경로의 값을 볼 수 있다.
    * @spec code:add_patha(Dir) => true | {error, bad_directory}
        새 디렉터리 Dir을 로드 경로의 앞에 추가
    * @spec code:add_pathz(Dir) => true | {error, bad_directory}
        새 디렉터리 Dir을 로드 경로의 끝에 추가
        