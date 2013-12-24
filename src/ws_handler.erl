-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    io:format("~p~p~n", ["new client connect ", self()]),
    main:manager() ! {connect, self()},
	{ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
	%%{reply, {text, << "server received: ", Msg/binary >>}, Req, State};
    manager ! {message, self(), mochijson2:decode(Msg)},
    {ok, Req, State};

websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
	{reply, {text, Msg}, Req, State};

websocket_info({_Ref, Msg}, Req, State) ->
	{reply, {text, mochijson2:encode(Msg)}, Req, State};

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    io:format("~p disconnected~n", [self()]),
    manager ! {disconnect, self()},
	ok.
