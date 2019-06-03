%%%-------------------------------------------------------------------
%% @doc conway_game public API
%% @end
%%%-------------------------------------------------------------------

-module(conway_game_app).

-behaviour(application).

-define(DEFAULT_SIZE, "500").
-define(DEFAULT_SLAVE_N, 3).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    case ?ROLE of
        master ->
            SlaveN = get_slave_n(),
            io:format("Slave cluster size: ~p\n", [SlaveN]),
            conway_master_sup:start_link(SlaveN);
        slave ->
            conway_slave_sup:start_link()
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

get_slave_n() ->
    {ok, SlaveN} = application:get_env(conway_game, slave_n),
    SlaveN.
