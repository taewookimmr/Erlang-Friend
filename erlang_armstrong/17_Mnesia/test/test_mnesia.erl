-module(test_mnesia).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-record(shop, {item, quantity, cost}).
-record(cost, {name, price}).
-record(design, {id, plan}).

do_this_once()->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(shop, [{attributes, record_info(fields, shop)}]),
    mnesia:create_table(cost, [{attributes, record_info(fields, cost)}]),
    mnesia:create_table(design, [{attributes, record_info(fields, design)}]),
    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([shop, cost, design], 20000).

%% 대응 sql
%% SELECT * FROM shop;

demo(select_shop)->
    do( qlc:q([X || X <- mnesia:table(shop)]) );


%% 대응 sql
%% SELECT shop.item, shop.quantity FROM shop;

demo(select_some)->
    do( qlc:q([{X#shop.item, X#shop.quantity} || X <- mnesia:table(shope)]));

%% 대응 sql
%% SELECT shop.item FROM shop
%% WHERE shop.quantity < 250;

demo(reorder) -> 
    do( qlc:q([X#shop.item || X <- mnesia:table(shop), X#shop.quantity < 250]));



%% 대응 sql
%% SELECT shop.item, shop.quantity, cost.name, cost.price 
%% FROM shop, cost
%% WHERE shop.item = cost.name
%%  AND cost.price < 2
%%  AND shop.quantity < 250 

demo(join) -> 
    do( qlc:q([X#shop.item || X <- mnesia:table(shop),
                                X#shop.quantity < 250,
                                Y <- mnesia:table(cost),
                                X#shop.item =:= Y#cost.name,
                                Y#cost.price < 2 ]));

demo(join_test) -> 
    do( qlc:q([X#shop.item || X <- mnesia:table(shop),
                            X#shop.quantity < 250,
                            Y <- mnesia:table(cost),
                            Y#cost.price < 2 ,
                            X#shop.item =:= Y#cost.name])).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.


example_tables()->
    [%% Ths shop table
        {shop, apple, 20, 2.3},
        {shop, orange, 100, 3.8},
        {shop, pear, 200, 3.6},
        {shop, banana, 420, 4.5},
        {shop, potato, 2456, 1.2},
    %% The cost table   
        {cost, apple, 1.5},
        {cost, oragne, 2.4},
        {cost, pear, 2.2},
        {cost, banana, 1.5},
        {cost, potato, 0.6}    
    ].


add_shop_item(Name, Quantity, Cost)->
    Row = #shop{item=Name, quantity=Quantity, cost=Cost},
    F = fun() ->
            mnesia:write(Row)
        end,
    mnesia:transaction(F).

remove_shop_item(Item)->
    Oid = {shop, Item},
    F = fun() -> 
            mnesia:delete(Oid)
        end,
    mnesia:transaction(F).

farmer(Nwant) ->
    %% Nwant 농부가 사려고 하는 오렌지의 개수
    F = fun() ->
        [Apple] = mnesia:read({shop, apple}),
        Napples = Apple#shop.quantity,
        Apple1 = Apple#shop{quantity = Napples + 2 * Nwant},
        %% database update
        mnesia:write(Apple1),
        
        [Orange] = mnesia:read({shop, orange}),
        NOranges = Orange#shop.quantity,
        if 
            NOranges >= Nwant -> 
                N1 = NOranges - Nwant,
                Orange1 = Orange#shop{quantity=N1},
                %% database update
                mnesia:write(Orange1);
            true ->
                %% when orange is not enough
                mnesia:abort(freak_on_a_leash)
        end
    end,
    mnesia:transaction(F).

reset_tables() ->
    mnesia:clear_table(shop),
    mnesia:clear_table(cost),
    F = fun() -> 
            foreach(fun mnesia:write/1, example_tables())
        end,
    mnesia:transaction(F).


add_plans() ->
    D1 = #design{id = {joe , 1},
                plan = {circle, 10}},
    D2 = #design{id = fred,
                plan = {rectangle, 10, 5}},
  
    F = fun() ->
            mnesia:write(D1),
            mnesia:write(D2)
        end,
    mnesia:transaction(F).

get_plan(PlanId) ->
    F = fun() -> mnesia:read({design, PlanId}) end,
    mnesia:transaction(F).


