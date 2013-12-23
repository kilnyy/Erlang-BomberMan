-module(bot).

-export([add/0, del/0]).

-define(Speed, 3).

bots() ->
    X = get(bots),
    if 
        X == undefined -> [];
        true -> X
    end.

add() -> 
    put(bots, [spawn(fun() -> start() end) | bots()]).
del() ->
    OldBots = bots(),
    if
        length(OldBots) > 0 ->
            [Pid | Bots] = OldBots,
            Pid ! {die},
            put(bots, Bots);
        true ->
            io:format("no bots now.")
    end.

start() -> 
    manager ! {connect, self()},
    name("Bot" ++ pid_to_list(self())),
    run(),
    loop().

move(Dx, Dy) ->
    send([<<"player">>, <<"move">>, Dx, Dy]).

set_bomb() ->
    send([<<"bomb">>, <<"set">>]).

name(Name) ->
    send([<<"name">>, list_to_binary(Name)]).

run() ->
    X = random:uniform(15),
    if
        X =< 3 -> move(-?Speed, 0);
        X =< 6 -> move(0, -?Speed);
        X =< 9 -> move(?Speed, 0);
        X =< 12 -> move(0, ?Speed);
        true -> move(0, 0)
    end,
    if
        X rem 4 == 0 -> set_bomb();
        true -> ok
    end,
    erlang:send_after(500, self(), {run}).
    
loop() ->
    receive
        {die} -> manager ! {disconnect, self()};
        {run} -> run(), loop()
        %%X -> io:format("server message to bot ~p: ~p~n", [self(), X])
    end.


send(Params) ->
    manager ! {message, self(), Params}.
