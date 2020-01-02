%%%-------------------------------------------------------------------
%%% @author taewookim
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 1월 2020 오전 5:18
%%%-------------------------------------------------------------------
-module(mon_http).
-author("taewookim").

%% API
-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) ->
  {ok, Req, no_state}.

handle(Req,  State) ->
  {Api, Req1} = cowboy_req:binding(api, Req),
  {What, Req2} = cowboy_req:binding(what, Req1),
  {Opt, Req3} = cowboy_req:binding(opt, Req2),
  %% Data loading
  {ok, Data, Req4} = cowboy_req:body_qs(Req3),

  io:format("good api=~p, what=~p, opt=~p ~n", [Api, What, Opt]),
  Reply = handle(Api, What, Opt, Data),

  {ok, Req5} = cowboy_req:reply(200,[
    {<<"content-type">>, <<"text/plain">>}
  ], Reply, Req4),
  {ok, Req5, State}.

handle(<<"login">>, _, _, Data) ->
  Id = proplists:get_value(<<"id">>, Data),
  Password = proplists:get_value(<<"password">>, Data),
  case mon_users:login(Id, Password) of
    ok ->
      <<"{\"result\":\"login ok\"}">>;
    _ ->
      <<"{\"result\":\"login fail\"}">>
    end;


handle(<<"join">>, _,_, Data) ->
  Id = proplists:get_value(<<"id">>, Data),
  Password = proplists:get_value(<<"password">>, Data),
  case mon_users:join(Id, Password) of
    fail ->
      <<"{\"result\":\"duplicated\"}">>;
    ok ->
      <<"{\"result\":\"join\"}">>
  end;


handle(<<"hello">>, <<"world">>,_,_) ->
  <<"{\"result\":\"Hello World\"}">>;

handle(_, _,_,_) ->
  <<"{\"result\":\"error\"}">>.



terminate(_Reason, _Req, _State) ->
  ok.