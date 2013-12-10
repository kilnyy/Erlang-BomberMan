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
            io:format("~p~n", [Params]),
            case Params of
                [<<"player">>, <<"move">>, _X, _Y, _Dx, _Dy, _TimeStamp] ->
                    game_manager ! {player, Client, tl(Params)};
                [<<"time">>, TimeStamp] ->
                    emit(Client, [<<"time">>, TimeStamp])
            end,
            loop(Clients)
    end.

current() ->
    {H, S, M} = erlang:now(),
    H * 1000000 * 1000 + S * 1000 + (M rem 1000).

emit(Client, Msg) ->
    Client ! {self(), json:encode(Msg ++ [current()])}.

broadcast([], _Msg) ->
    ok;
broadcast([Client | Tail], Msg) ->
    emit(Client, Msg),
    broadcast(Tail, Msg);
broadcast(Clients, Msg) ->
    broadcast(gb_sets:to_list(Clients), Msg).
