%% @private
-module(bomber_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, bomber, "index.html"}},
			{"/websocket", ws_handler, []},
			{"/static/[...]", cowboy_static, {priv_dir, bomber, "static"}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 2333}],
		[{env, [{dispatch, Dispatch}]}]),
	bomber_sup:start_link().

stop(_State) ->
	ok.
