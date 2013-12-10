-module(player).

-export([start/2]).

start(Client, Id) ->
    game_manager ! {player_init, Client, Id},
    receive_loop(Client, Id, 0, 0, 0, 0).

receive_loop(Client, Id, X, Y, Dx, Dy) ->
    receive
        {disconnect} ->
            ok;
        {move, [Nx, Ny, NDx, NDy]} ->
            receive_loop(Client, Id, Nx, Ny, NDx, NDy);
        {run} ->
            game_manager ! {player_state, Client, Id, X+Dx, Y+Dy, Dx, Dy},
            receive_loop(Client, Id, X+Dx, Y+Dy, Dx, Dy)
    end.
