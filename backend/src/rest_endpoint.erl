-module(rest_endpoint).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([content_types_accepted/2]).
-export([allowed_methods/2]).
-export([to_json/2]).

init(Req, Opts) ->
  ReqWithCORS = add_cors_headers(Req),
	{cowboy_rest, ReqWithCORS, Opts}.

allowed_methods(Req, State) -> {[<<"POST">>, <<"GET">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, to_json}
	], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, to_json}
     ], Req, State}.

to_json(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            start_the_game(Req, State);
        <<"GET">> ->
            get_state(Req, State);
        <<"OPTIONS">> ->
            {true, Req, State} 
    end.

start_the_game(Req, State) ->
    {ok, Data, Req2} = read_body(Req, <<>>),
    BodyMap = jiffy:decode(Data, [return_maps]),
    Size = maps:get(<<"size">>, BodyMap),
    Fields = communication_utils:fields_from_maps(maps:get(<<"fields">>, BodyMap)),
    GoalGenerations = maps:get(<<"goalGenerations">>, BodyMap),
    Board = rect:new(Size, Size, 0, Fields),
    case is_master_node_started() of
        false ->
            conway_master_worker:run_for_board(Board, GoalGenerations),
            GameState = conway_master_worker:get_state(),
            ws_h:update_state(GameState),
            {true, Req2, State};
        true ->
            % TODO maybe implement restarting for different board(?)
            io:format("Already started!\n"),
            {false, Req2, State}
    end.

get_state(Req, State) ->
    Resp = conway_master_worker:get_state(),
    Json = communication_utils:game_state_to_json(Resp),
     {Json, Req, State}.



read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

to_int(B) when is_binary(B) ->
    list_to_integer(binary_to_list(B));
to_int(B) when is_list(B) ->
    list_to_integer(B).

%% Helpers

add_cors_headers(Req) ->
  Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
  cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"Content-Type, Access-Control-Allow-Headers, Authorization, X-Requested-With">>, Req1).

is_master_node_started() ->
    case maps:get(start_state, conway_master_worker:get_state()) of
        computing ->
            true;
        _ ->
            false
    end.
