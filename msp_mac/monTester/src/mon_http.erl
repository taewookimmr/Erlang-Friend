%%%-------------------------------------------------------------------
%%% @author taewookim
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 1월 2020 8:54 오후
%%%-------------------------------------------------------------------
-module(mon_http).
-author("taewookim").

%% API
-export([init/3, handle/2, terminate/3]).

init(_Type, Req, [])->
	{ok, Req, no_state}.

handle(Req, State) ->
	{ok, Req2} = cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain">>}
	], <<"{\"result\":Hello world!\"}">>, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State)->
	ok.