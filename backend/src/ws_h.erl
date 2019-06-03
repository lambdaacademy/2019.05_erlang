-module(ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([update_state/1]).
-export([terminate/3]).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts, #{idle_timeout => infinity}}.

update_state(GameState) ->
    Clients = ets:tab2list(ws_clients),
    lists:map(fun({C, _}) -> C ! {send_state, GameState} end, Clients).

websocket_init(State) ->
    ets:insert(ws_clients, {self(), a}),
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    {reply, {text, << "That's what she said! ", Msg/binary >>}, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({send_state, GameState}, State) ->
    Resp = communication_utils:game_state_to_json(GameState),
    {reply, {text, Resp}, State}.

terminate(_Reason, _Req, _State) ->
    io:format("[WARN] Disconnecting the client! ~p\n", [self()]),
    ets:delete(ws_clients, self()).
