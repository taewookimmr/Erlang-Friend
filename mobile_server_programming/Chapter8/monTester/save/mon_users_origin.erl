%%%-------------------------------------------------------------------
%%% @author taewookim
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 1월 2020 오후 7:53
%%%-------------------------------------------------------------------
-module(mon_users_origin).
-author("taewookim").
-include("mon_record.hrl").
%% API
-export([join/2, login/2, point/2, new_session/1, loop/1, make_session_key/2]).


join(Id, Password)->
    F = fun() ->
        case mnesia:read(users, Id) of
            [] ->
                %% 해당 Id로 가입된 데이터가 없으면 저장한다.
                Users = #users{id=Id, password=Password},
                ok = mnesia:write(Users); %% 가입 완료
            _ ->
                fail
        end
        end,
    mnesia:activity(transaction, F).

login(Id, Password) ->
    F = fun() ->
        case mnesia:read(users, Id) of
            [U = #users{password=Password}] ->
                SessionKey = new_session(Id),
                {ok, SessionKey};
            _ ->
                fail
        end
    end,
    mnesia:activity(transaction, F).

point(SessionKey, Point)->
    case ets:lookup(session_list, SessionKey) of
        [{SessionKey, Pid}]->
            Ref = make_ref(),
            Pid ! {self(), Ref, save_point, Point},
            receive
                {Ref, saved}->
                    ok;
                _ ->
                    fail
            after 3000 ->
                fail
            end;
        _ ->
            fail
    end.

%% user session process
new_session(Id)->
    Pid = spawn(mon_users_origin, loop, [Id]),
    make_session_key_temp(Id, Pid).

%% session loop
loop(Id) ->
    receive
        {Pid, Ref, save_point, Point} ->
            save_point(Id, Point),
            Pid ! {Ref, saved};
        _ ->
            pass
    end,
    loop(Id).


%% session key
make_session_key(Id, Pid) ->
    %% seed initialization
    {A1, A2, A3} =  now(),
    random:seed(A1,A2,A3),

    %% 1~10000 숫자 중 하나를 랜덤 선택
    Num = random:uniform(10000),

    Hash = erlang:phash2(Id),

    %% 두개의 값을 16진수로 조합하여 session key  생성
    io:format("cupcake"),
    List = io_lib:format("~.16B~.16B", [Hash, Num]),
    SessionKey = list_to_binary(lists:append(List)),

    %% 세션 키 저장 및 리턴
    ets:insert(session_list, {SessionKey, Pid}),
    SessionKey.

make_session_key_temp(Id, Pid)->
    ets:insert(session_list, {12345, Pid}),
    12345.


%% 유저 점수 저장
save_point(Id, Point) ->
    F = fun() ->
        case mnesia:read(users, Id) of
            [U] ->
                %% user 점수 저장
                Users = U#users{point=Point},
                ok = mnesia:write(Users);
            _ ->
                fail %% 저장 실패
        end
    end,
    mnesia:activity(transaction, F).
