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
    {ok, Req, []}.

websocket_handle({text, Msg}, Req, State) ->
    NewState = case Msg of
		   <<>> -> State;
		   Msg -> [Msg, << "\n" >> |State]
	       end,
    io:format("Msg: ~p; State: ~p; NewState: ~p~n", [Msg, State, NewState]),
    {reply, {text, NewState}, Req, NewState};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
