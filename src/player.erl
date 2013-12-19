-module(player).

-export([start/1]).

start(Client) ->
    game_manager ! {player_init, Client},
    loop(Client, 0, 0, 0, 0, 4, 0, 3).

move(Client, Nx, Ny, NDx, NDy, MaxBomb, CurBomb, Range, Delta) when abs(Delta) < 0.001 ->
    game_manager ! {player_state, Client, Nx, Ny, NDx, NDy},
    loop(Client, Nx, Ny, NDx, NDy, MaxBomb, CurBomb, Range);
move(Client, Nx, Ny, NDx, NDy, MaxBomb, CurBomb, Range, Delta) ->
    Rx = Nx + NDx * Delta / 1000,
    Ry = Ny + NDy * Delta / 1000,
    Ok = game_manager:check_move(Nx, Ny, Rx, Ry),
    if
        Ok ->
            game_manager ! {player_state, Client, Rx, Ry, NDx, NDy},
            loop(Client, Rx, Ry, NDx, NDy, MaxBomb, CurBomb, Range);
        true ->
            move(Client, Nx, Ny, NDx, NDy, MaxBomb, CurBomb, Range, Delta/2)
    end.

loop(Client, X, Y, Dx, Dy, MaxBomb, CurBomb, Range) ->
    receive
        {disconnect} ->
            ok;
        {move, [Nx, Ny, NDx, NDy, TimeStamp]} ->
            Cur = main:current(),
            move(Client, Nx, Ny, NDx, NDy, MaxBomb, CurBomb, Range, Cur-TimeStamp);
        {bomb, set, [Bx, By, TimeStamp]} ->
            if
                MaxBomb > CurBomb ->
                    spawn(bomb, start, [self(), Bx, By, Range, TimeStamp]),
                    loop(Client, X, Y, Dx, Dy, MaxBomb, CurBomb+1, Range);
                true ->
                    loop(Client, X, Y, Dx, Dy, MaxBomb, CurBomb, Range)
            end;
        {bomb, bang} ->
            loop(Client, X, Y, Dx, Dy, MaxBomb, CurBomb-1, Range);
        {bomb, bang, Nx, Ny, Range} ->
            Ok = game_manager:check_bomb(X, Y, Nx, Ny, Range),
            if
                Ok ->
                    start(Client);
                true ->
                    loop(Client, X, Y, Dx, Dy, MaxBomb, CurBomb, Range)
            end;
        {run} ->
            Nx = X + Dx / 10,
            Ny = Y + Dy / 10,
            Ok = game_manager:check_move(X, Y, Nx, Ny),
            if
                Ok ->
                    game_manager ! {player_state, Client, Nx, Ny, Dx, Dy},
                    loop(Client, Nx, Ny, Dx, Dy, MaxBomb, CurBomb, Range);
                true ->
                    game_manager ! {player_state, Client, X, Y, Dx, Dy},
                    loop(Client, X, Y, Dx, Dy, MaxBomb, CurBomb, Range)
            end
    end.
