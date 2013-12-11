-module(player).

-export([start/2]).

start(Client, Id) ->
    game_manager ! {player_init, Client, Id},
    receive_loop(Client, Id, 0, 0, 0, 0).

receive_loop(Client, Id, X, Y, Dx, Dy) ->
    receive
        {disconnect} ->
            ok;
        {move, [Nx, Ny, NDx, NDy, TimeStamp]} ->
            Cur = main:current(),
            Rx = Nx + NDx * (Cur - TimeStamp) / 1000,
            Ry = Ny + NDy * (Cur - TimeStamp) / 1000,
            game_manager ! {player_state, Client, Id, Rx, Ry, NDx, NDy},
            receive_loop(Client, Id, Rx, Ry, NDx, NDy);
        {run} ->
            game_manager ! {player_state, Client, Id, X+Dx/10, Y+Dy/10, Dx, Dy},
            receive_loop(Client, Id, X+Dx/10, Y+Dy/10, Dx, Dy)
    end.
