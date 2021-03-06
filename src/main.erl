-module(main).

-export([start/0, emit/2, broadcast/2, current/0, manager/0]).

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
        {message, Client, Params} ->
            %io:format("receive: ~p~n", [Params]),
            case Params of
                %For bot
                [<<"name">>, _Name] ->
                    game_manager ! {name, Client, tl(Params) ++ [current()]};
                [<<"player">>, <<"move">>, _Dx, _Dy] ->
                    game_manager ! {player, Client, tl(Params) ++ [current()]};
                [<<"bomb">>, <<"set">>] ->
                    game_manager ! {bomb, Client, tl(Params) ++ [current()]}; 

                %For client
                [<<"name">>, _Name, _TimeStamp] ->
                    game_manager ! {name, Client, tl(Params)};
                [<<"player">>, <<"move">>, _X, _Y, _Dx, _Dy, _TimeStamp] ->
                    game_manager ! {player, Client, tl(Params)};
                [<<"bomb">>, <<"set">>, _X, _Y, _Timestamp] ->
                    game_manager ! {bomb, Client, tl(Params)}; 
                [<<"time">>, TimeStamp] ->
                    emit(Client, [<<"time">>, TimeStamp])
            end,
            loop(Clients)
    end.

current() ->
    {H, S, M} = os:timestamp(),
    H * 1000000 * 1000 + S * 1000 + (M div 1000).

emit(Client, Msg) ->
    Client ! {self(), Msg ++ [current()]}.

broadcast([], _Msg) ->
    ok;
broadcast([Client | Tail], Msg) ->
    emit(Client, Msg),
    broadcast(Tail, Msg);
broadcast(Clients, Msg) ->
    broadcast(gb_sets:to_list(Clients), Msg).

manager() ->
    Manager = whereis(manager),
    if 
        Manager == undefined ->
            register(manager, spawn(main, start, []));
        true ->
            ok
    end,
    manager.
