%% @private
-module(bomberman_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, bomberman, "index.html"}},
			{"/websocket", ws_handler, []},
			{"/static/[...]", cowboy_static, {priv_dir, bomberman, "static"}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 2333}],
		[{env, [{dispatch, Dispatch}]}]),
	bomberman_sup:start_link().

stop(_State) ->
	ok.
