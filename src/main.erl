-module(main).

-export([start/0, emit/2, broadcast/2]).

start() ->
    register(game_manager, spawn(game_manager, start, [])),
    loop(gb_sets:new()).

loop(Clients) ->
    receive
        {connect, Client} ->
            game_manager ! {connect, Client},
            loop(gb_sets:add(Client, Clients));
        {disconnect, Client} ->
            game_manager ! {disconnect, Client},
            loop(gb_sets:delete(Client, Clients));
        {message, Client, Msg} ->
            Params = json:decode(Msg),
            case Params of
                [<<"player">>, _X, _Y, _Dx, _Dy] ->
                    game_manager ! {player, Client, tl(Params)}
            end,
            loop(Clients)
    end.

emit(Client, Msg) ->
    Client ! {self(), Msg}.

broadcast([], _Msg) ->
    ok;
broadcast([Client | Tail], Msg) ->
    Client ! {self(), Msg},
    broadcast(Tail, Msg);
broadcast(Clients, Msg) ->
    broadcast(gb_sets:to_list(Clients), Msg).
