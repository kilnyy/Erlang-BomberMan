-module(game_manager).

-export([start/0]).

start() ->
    erlang:start_timer(100, self(), run),
    loop(dict:new()).

loop(ClientsDict) ->
    receive
        {connect, Client} ->
            Id = dict:size(ClientsDict),
            Player = spawn(player, start, [Id]),
            loop(dict:store(Client, Player, ClientsDict));
        {disconnect, Client} ->
            Player = dict:fetch(Client, ClientsDict),
            Player ! {disconnect},
            loop(dict:erase(Client, ClientsDict));
        {player, Client, Params} ->
            Player = dict:fetch(Client, ClientsDict),
            Player ! {move, Params},
            loop(ClientsDict);
        {player_state, Id, X, Y, Dx, Dy} ->
            Clients = dict:fetch_keys(ClientsDict),
            Msg = json:encode([<<"player">>, Id, X, Y, Dx, Dy]),
            main:broadcast(Clients, Msg),
            loop(ClientsDict);
        {timeout, _Ref, Msg} ->
            run(ClientsDict),
            erlang:start_timer(100, self(), Msg),
            loop(ClientsDict);
        X ->
            io:format("~p~n", [X])
    end.


run(ClientsDict) ->
    dict:map(fun(_Client, Player) -> Player ! {run} end, ClientsDict),
    ok.
