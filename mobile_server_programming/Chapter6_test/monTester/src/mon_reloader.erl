%%%-------------------------------------------------------------------
%%% @author taewookim
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 1월 2020 오전 4:35
%%%-------------------------------------------------------------------
-module(mon_reloader).
-author("taewookim").

-include_lib("kernel/include/file.hrl").
%% API
-export([start/0, loop/1, reload/1]).

start()->
    Pid = spawn(mon_reloader, loop, [erlang:localtime()]),
    timer:send_interval(timer:seconds(1), Pid, check). % Pid에게 check 메시지를 1초 간격으로 보내겠다는 의미

loop(From)->
    receive
        check->
            To = erlang:localtime(),
            [check(From, To, Module, Filename) || {Module, Filename} <- code:all_loaded(), is_list(Filename)],
            loop(To);
        update->
            ?MODULE:loop(From); % 아직 까지는 프로세스에서 수동으로 update를 보내주어야 하는 단점. 6장에서 보완한다.
        Other ->
            io:format("~p~n",[Other]),
            loop(From)

    end.

check(From, To, Module, Filename)->
    case file:read_file_info(Filename) of
        {ok, #file_info{mtime=MTime}} when MTime >= From, MTime < To ->
            reload(Module);
        _ ->
            pass
    end.

reload(Module) ->
    io:format("Reloading ~p ...", [Module]),
    code:purge(Module),
    code:load_file(Module),
    io:format(" ok. ~n" ).

