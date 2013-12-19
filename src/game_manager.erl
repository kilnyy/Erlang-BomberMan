-module(game_manager).

-export([start/0, game_state/1, game_state/2, check_block/2, check_bomb/5, check_move/4]).

start() ->
    erlang:start_timer(100, self(), run),
    ets:new(gameState, [named_table]),
    game_state(clientsDict, dict:new()),
    game_state(bombs, dict:new()),
    game_state(blocks, dict:new()),
    loop().

loop() ->
    ClientsDict = game_state(clientsDict),
    Bombs = game_state(bombs),
    receive
        {connect, Client} ->
            Player = spawn(player, start, [Client]),
            game_state(clientsDict, dict:store(Client, Player, ClientsDict));
        {disconnect, Client} ->
            Clients = dict:fetch_keys(ClientsDict),
            Msg = [<<"disconnect">>, id(Client)],
            main:broadcast(Clients, Msg),
            game_state(clientsDict, dict:erase(Client, ClientsDict));

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
            game_state(bombs, dict:store({X, Y}, Bomb, Bombs)),
            Msg = [<<"bomb">>, <<"set">>, id(Bomb), X, Y, Range],
            main:broadcast(Clients, Msg);
        {bomb_bang, Bomb, X, Y, Range} ->
            Clients = dict:fetch_keys(ClientsDict),
            game_state(bombs, dict:erase({X, Y}, Bombs)),
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

check_block(X, Y) ->
    Bombs = game_state(bombs),
    Blocks = game_state(blocks),
    Pos = dict:fetch_keys(Bombs) ++ dict:fetch_keys(Blocks),
    lists:all(fun({Px, Py}) -> (abs(Px-X) > 0.5) or (abs(Py-Y) > 0.5) end, Pos).

check_bomb(X, Y, Nx, Ny, Range) ->
    Max = max(abs(Nx-X), abs(Ny-Y)),
    Min = min(abs(Nx-X), abs(Ny-Y)),
    (Min < 0.5) and (Max < Range + 0.5).

check_move(X, Y, Nx, Ny) ->
    game_manager:check_block(Nx, Ny) orelse ((round(X)==round(Nx)) andalso round(Y)==round(Ny)).

id(Pid) ->
    list_to_binary(pid_to_list(Pid)).

run(ClientsDict) ->
    dict:map(fun(_Client, Player) -> Player ! {run} end, ClientsDict),
    ok.

game_state(Key) ->
    [{Key, Res}] = ets:lookup(gameState, Key),
    Res.

game_state(Key, Value) ->
    true = ets:insert(gameState, {Key, Value}).
