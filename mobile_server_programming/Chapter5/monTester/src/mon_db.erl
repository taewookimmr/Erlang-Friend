%%%-------------------------------------------------------------------
%%% @author taewookim
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 1월 2020 오후 7:41
%%%-------------------------------------------------------------------
-module(mon_db).
-author("taewookim").
-include("mon_record.hrl").

%% API
-export([install/0, uninstall/0]).

install() ->
  ok = mnesia:create_schema([node()]),
  application:start(mnesia),
  mnesia:create_table(users, [{attributes, record_info(fields, users)}, {disc_copies, [node()]}]),
  application:stop(mnesia).

uninstall()->
  application:stop(mnesia),
  mnesia:delete_schema(node()).
