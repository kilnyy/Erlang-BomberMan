-module(player).

-export([start/1]).

start(Id) ->
    receive_loop(Id, 0, 0, 0, 0).

receive_loop(Id, X, Y, Dx, Dy) ->
    receive
        {disconnect} ->
            ok;
        {move, [_X, _Y, Nx, Ny]} ->
            receive_loop(Id, X, Y, Nx, Ny);
        {run} ->
            game_manager ! {player_state, Id, X+Dx, Y+Dy, Dx, Dy},
            receive_loop(Id, X+Dx, Y+Dy, Dx, Dy)
    end.
