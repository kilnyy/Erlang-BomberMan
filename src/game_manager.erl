-module(game_manager).

-export([start/0, gameState/1, gameState/2]).

start() ->
    erlang:start_timer(100, self(), run),
    ets:new(gameState, [named_table]),
    gameState(clientsDict, dict:new()),
    gameState(bombs, dict:new()),
    loop().

loop() ->
    ClientsDict = gameState(clientsDict),
    Bombs = gameState(bombs),
    receive
        {connect, Client} ->
            Player = spawn(player, start, [Client]),
            gameState(clientsDict, dict:store(Client, Player, ClientsDict));
        {disconnect, Client} ->
            Clients = dict:fetch_keys(ClientsDict),
            Msg = [<<"disconnect">>, id(Client)],
            main:broadcast(Clients, Msg),
            gameState(clientsDict, dict:erase(Client, ClientsDict));

        %% From Client
        {player, Client, Params} ->
            Player = dict:fetch(Client, ClientsDict),
            Player ! {move, tl(Params)};
        {bomb, Client, Params} ->
            Player = dict:fetch(Client, ClientsDict),
            Player ! {bomb, set, tl(Params)};
        
        %% From Server
        {player_state, Client, X, Y, Dx, Dy} ->
            Clients = dict:fetch_keys(ClientsDict),
            Msg = [<<"player">>, <<"move">>, id(Client), X, Y, Dx, Dy],
            main:broadcast(Clients, Msg);
        {player_init, Client} ->
            Msg = [<<"player">>, <<"init">>, id(Client)],
            main:emit(Client, Msg);
        {bomb_set, Bomb, X, Y, Range} ->
            Clients = dict:fetch_keys(ClientsDict),
            gameState(bombs, dict:store({X, Y}, Bomb, Bombs)),
            Msg = [<<"bomb">>, <<"set">>, id(Bomb), X, Y, Range],
            main:broadcast(Clients, Msg);
        {bomb_bang, Bomb, X, Y, Range} ->
            Clients = dict:fetch_keys(ClientsDict),
            gameState(bombs, dict:erase({X, Y}, Bombs)),
            dict:map(fun(_Pos, NBomb) -> NBomb ! {bomb, bang, X, Y, Range} end, Bombs),
            dict:map(fun(_Client, Player) -> Player ! {bomb, bang, X, Y, Range} end, ClientsDict),
            Msg = [<<"bomb">>, <<"bang">>, id(Bomb)],
            main:broadcast(Clients, Msg);
            

        {timeout, _Ref, Msg} ->
            run(ClientsDict),
            erlang:start_timer(100, self(), Msg);
        Msg ->
            io:format("game_manager unaccepted: ~p~n", [Msg])
    end,
    loop().

id(Pid) ->
    list_to_binary(pid_to_list(Pid)).

run(ClientsDict) ->
    dict:map(fun(_Client, Player) -> Player ! {run} end, ClientsDict),
    ok.

gameState(Key) ->
    [{Key, Res}] = ets:lookup(gameState, Key),
    Res.

gameState(Key, Value) ->
    true = ets:insert(gameState, {Key, Value}).
