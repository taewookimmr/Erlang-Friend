%%%-------------------------------------------------------------------
%%% @author taewookim
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 1월 2020 오후 4:58
%%%-------------------------------------------------------------------
-module(chat_manager).
-author("taewookim").

-behaviour(gen_server).

%% API
-export([start_link/0, init_room/1, get_room/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {room_list}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init_room(Number)->
    gen_server:call(?MODULE, {init_room, Number}).

get_room()->
    gen_server:call(?MODULE, {get_room}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_call({init_room, Number}, _From, State)->
    %% atome 형식의 room_list 생성
    RoomList = lists:map(
        fun(N) ->
            Room = lists:flatten(io_lib:format("room~b", [N])),
            list_to_atom(Room)
        end,
        lists:seq(1, Number)),
    start_chat_admin(RoomList),
    State1 = State#state{room_list=RoomList},
    {reply, RoomList, State1};

handle_call({get_room}, _From, State)->
    %% 첫 번째 룸을 가져옥
    [Room|List] = State#state.room_list,
    %% 다시 맨 뒤로 보낸다.
    NewList = lists:append(List, [Room]),
    %% 저장 후 Room을 reply
    State1 = State#state{room_list=NewList},
    {reply, Room, State1};

handle_call(_Request, _From, State)->
    {reply, ok, State}.


%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State = #state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State = #state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.


start_chat_admin([]) ->
    ok;
start_chat_admin([Room|List]) ->
    supervisor:start_child(room_sup, [Room]),
    start_chat_admin(List).


%%%===================================================================
%%% Internal functions
%%%===================================================================

