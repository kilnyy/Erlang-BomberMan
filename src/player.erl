-module(player).

-export([start/1]).
-define(LimitX, 10.45).
-define(LimitY, 10.45).

start(Client) ->
    game_manager ! {player_init, Client},
    loop(Client, 0, 0, 0, 0, 4, 0, 4).

start(Client, CurBomb) ->
    game_manager ! {player_init, Client},
    loop(Client, 0, 0, 0, 0, 4, CurBomb, 4).

move(Client, Nx, Ny, NDx, NDy, MaxBomb, CurBomb, Range, Delta) when abs(Delta) < 0.001 ->
    game_manager ! {player_state, Client, Nx, Ny, 0, 0},
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

in_range(X, R) ->
    if 
        X > R -> R;
        X < -R -> -R;
        true -> X
    end.

loop(Client, X, Y, Dx, Dy, MaxBomb, CurBomb, Range) when (abs(X) > ?LimitX) or (abs(Y) > ?LimitY) ->
    loop(Client, in_range(X, ?LimitX), in_range(Y, ?LimitY), Dx, Dy, MaxBomb, CurBomb, Range);
loop(Client, X, Y, Dx, Dy, MaxBomb, CurBomb, Range) ->
    receive
        {disconnect} ->
            ok;
        {move, [NDx, NDy, TimeStamp]} ->
            Cur = main:current(),
            move(Client, X, Y, NDx, NDy, MaxBomb, CurBomb, Range, Cur-TimeStamp);
        {bomb, set, [TimeStamp]} ->
            Ok = (MaxBomb > CurBomb) and (game_manager:check_block(X, Y)),
            if
                Ok ->
                    spawn(bomb, start, [self(), round(X), round(Y), Range, TimeStamp]),
                    loop(Client, X, Y, Dx, Dy, MaxBomb, CurBomb+1, Range);
                true ->
                    loop(Client, X, Y, Dx, Dy, MaxBomb, CurBomb, Range)
            end;
        {move, [Nx, Ny, NDx, NDy, TimeStamp]} ->
            Cur = main:current(),
            move(Client, Nx, Ny, NDx, NDy, MaxBomb, CurBomb, Range, Cur-TimeStamp);
        {bomb, set, [Bx, By, TimeStamp]} ->
            Ok = (MaxBomb > CurBomb) and (game_manager:check_block(X, Y)),
            if
                Ok ->
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
                    start(Client, CurBomb);
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
                    game_manager ! {player_state, Client, X, Y, 0, 0},
                    loop(Client, X, Y, Dx, Dy, MaxBomb, CurBomb, Range)
            end
    end.
