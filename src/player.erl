-module(player).

-export([start/1]).

start(Client) ->
    game_manager ! {player_init, Client},
    loop(Client, 0, 0, 0, 0, 4, 0, 3).

loop(Client, X, Y, Dx, Dy, MaxBomb, CurBomb, Range) ->
    receive
        {disconnect} ->
            ok;
        {move, [Nx, Ny, NDx, NDy, TimeStamp]} ->
            Cur = main:current(),
            Rx = Nx + NDx * (Cur - TimeStamp) / 1000,
            Ry = Ny + NDy * (Cur - TimeStamp) / 1000,
            game_manager ! {player_state, Client, Rx, Ry, NDx, NDy},
            loop(Client, Rx, Ry, NDx, NDy, MaxBomb, CurBomb, Range);
        {bomb, set, [Bx, By, _TimeStamp]} ->
            if
                MaxBomb > CurBomb ->
                    spawn(bomb, start, [self(), Bx, By, Range]),
                    loop(Client, X, Y, Dx, Dy, MaxBomb, CurBomb+1, Range);
                true ->
                    loop(Client, X, Y, Dx, Dy, MaxBomb, CurBomb, Range)
            end;
        {bomb, bang} ->
            loop(Client, X, Y, Dx, Dy, MaxBomb, CurBomb-1, Range);
        {bomb, bang, Nx, Ny, Range} ->
            Dis = max(abs(Nx-round(X)), abs(Ny-round(Y))),
            if
                (Nx == round(X)) or (Ny == round(Y)) and (Dis =< Range) ->
                    start(Client);
                true ->
                    loop(Client, X, Y, Dx, Dy, MaxBomb, CurBomb, Range)
            end;
        {run} ->
            game_manager ! {player_state, Client, X+Dx/10, Y+Dy/10, Dx, Dy},
            loop(Client, X+Dx/10, Y+Dy/10, Dx, Dy, MaxBomb, CurBomb, Range)
    end.
