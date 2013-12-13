-module(game_manager).

-export([start/0]).

start() ->
    erlang:start_timer(100, self(), run),
    loop(dict:new()).

loop(ClientsDict) ->
    receive
        {connect, Client} ->
            Player = spawn(player, start, [Client]),
            loop(dict:store(Client, Player, ClientsDict));
        {disconnect, Client} ->
            Clients = dict:fetch_keys(ClientsDict),
            Msg = [<<"disconnect">>, id(Client)],
            main:broadcast(Clients, Msg),
            loop(dict:erase(Client, ClientsDict));

        %% From Client
        {player, Client, Params} ->
            Player = dict:fetch(Client, ClientsDict),
            Player ! {move, tl(Params)},
            loop(ClientsDict);
        {bomb, Client, Params} ->
            Player = dict:fetch(Client, ClientsDict),
            Player ! {bomb, set, tl(Params)},
            loop(ClientsDict);
        
        %% From Server
        {player_state, Client, X, Y, Dx, Dy} ->
            Clients = dict:fetch_keys(ClientsDict),
            Msg = [<<"player">>, <<"move">>, id(Client), X, Y, Dx, Dy],
            main:broadcast(Clients, Msg),
            loop(ClientsDict);
        {player_init, Client} ->
            Msg = [<<"player">>, <<"init">>, id(Client)],
            main:emit(Client, Msg),
            loop(ClientsDict);
        {bomb_set, Bomb, X, Y, Range} ->
            Clients = dict:fetch_keys(ClientsDict),
            Msg = [<<"bomb">>, <<"set">>, id(Bomb), X, Y, Range],
            main:broadcast(Clients, Msg),
            loop(ClientsDict);
        {bomb_bang, Bomb, _X, _Y, _Range} ->
            Clients = dict:fetch_keys(ClientsDict),
            Msg = [<<"bomb">>, <<"bang">>, id(Bomb)],
            main:broadcast(Clients, Msg),
            loop(ClientsDict);
            

        {timeout, _Ref, Msg} ->
            run(ClientsDict),
            erlang:start_timer(100, self(), Msg),
            loop(ClientsDict);
        Msg ->
            io:format("game_manager unaccepted: ~p~n", [Msg]),
            loop(ClientsDict)
    end.

id(Pid) ->
    list_to_binary(pid_to_list(Pid)).

run(ClientsDict) ->
    dict:map(fun(_Client, Player) -> Player ! {run} end, ClientsDict),
    ok.
