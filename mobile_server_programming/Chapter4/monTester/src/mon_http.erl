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

  io:format("Pistol grip pump api=~p, what=~p, opt=~p ~n", [Api, What, Opt]),

  {ok, Req4} = cowboy_req:reply(200,[
    {<<"content-type">>, <<"text/plain">>}
  ], <<"{\"result\":\"Hello World\"}">>, Req3),
  {ok, Req4, State}.


terminate(_Reason, _Req, _State) ->
  ok.