%%%-------------------------------------------------------------------
%%% @author taewookim
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 1월 2020 오후 7:53
%%%-------------------------------------------------------------------
-module(mon_users).
-author("taewookim").
-include("mon_record.hrl").
%% API
-export([join/2, login/2]).


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
        ok;
      _ ->
        fail
    end
  end,
  mnesia:activity(transaction, F).