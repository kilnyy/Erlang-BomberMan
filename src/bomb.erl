-module(bomb).

-export([start/5]).

start(Player, X, Y, Range, Timestamp) ->
  game_manager ! {bomb_set, self(), X, Y, Range},
  erlang:send_after(3000 - main:current() + Timestamp, self(), {bang}),
  loop(Player, X, Y, Range).

bang(Player, X, Y, Range) ->
  Player ! {bomb, bang},
  game_manager ! {bomb_bang, self(), X, Y, Range}.

loop(Player, X, Y, Range) ->
    receive
        {bang} ->
            bang(Player, X, Y, Range);
        {bomb, bang, Nx, Ny, Nrange}->
            Ok = game_manager:check_bomb(X, Y, Nx, Ny, Nrange),
            if
                Ok ->
                    bang(Player, X, Y, Range);
                true ->
                    loop(Player, X, Y, Range)
            end
end.
