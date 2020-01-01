%%%-------------------------------------------------------------------
%%% @author taewookim
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 1월 2020 오전 3:15
%%%-------------------------------------------------------------------
-module(mon_app).
-author("taewookim").

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  %% 필요한 어플리케이션 실행
  ok = application:start(crypto),
  ok = application:start(cowlib),
  ok = application:start(ranch),
  ok = application:start(cowboy),

  %% Cowboy router 설정
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/:api/[:what/[:opt]]", mon_http, []}
    ]}
  ]),

  %% Http server 실행
  {ok, _} = cowboy:start_http(http, 100, [{port, 6060}], [
    {env, [{dispatch, Dispatch}]}
  ]),

  %% Code_reloader 실행
  mon_reloader:start(),
  case mon_sup:start_link() of
    {ok, Pid} ->
      io:format("start ok~n"),
      {ok, Pid};

    Error ->
      Error

  end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
