-module(bomb).

-export([start/4]).

start(Player, X, Y, Range) ->
  game_manager ! {bomb_set, self(), X, Y, Range},
  erlang:send_after(3000, self(), {bang}),
  loop(Player, X, Y, Range).

bang(Player, X, Y, Range) ->
  Player ! {bomb, bang},
  game_manager ! {bomb_bang, self(), X, Y, Range}.

loop(Player, X, Y, Range) ->
  receive
    {bang} ->
      bang(Player, X, Y, Range);
    {bomb, bang, Nx, Ny, Nrange}->
      Dis = max(abs(Nx-X), abs(Ny-Y)),
      if
        (Nx == X) or (Ny == Y) and (Dis =< Nrange)->
          bang(Player, X, Y, Range);
        true ->
          loop(Player, X, Y, Range)
      end
  end.
