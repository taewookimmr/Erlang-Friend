%%%-------------------------------------------------------------------
%%% @author taewookim
%%% @copyright (C) <COMPANY>
%%% @doc
%%%
%%% @end
%%%
%%%-------------------------------------------------------------------
-author("taewookim").

-record(users, {
    id,
    password,
    token,
    level=0,
    exp=0,
    point=0
}).
