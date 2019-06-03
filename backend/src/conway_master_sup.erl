%%%-------------------------------------------------------------------
%% @doc conway_game top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(conway_master_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(SlaveN) ->
    start_rest(),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [SlaveN]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([SlaveN]) ->
    ConwayWorker = #{id => conway_master_worker, start => {conway_master_worker, start_link, [SlaveN]}},
    {ok, { {one_for_all, 0, 1}, [ConwayWorker]} }.

%%====================================================================
%% Internal functions
%%====================================================================
start_rest() ->
    % TODO replace TableHolder
    TableHolder = spawn(fun loop/0),
    ets:new(ws_clients, [public, {heir, TableHolder, none}, named_table]),
    io:format("~p", [ets:tab2list(ws_clients)]),
    Dispatch = cowboy_router:compile([
		{'_', [
			{"/", rest_endpoint, []},
                        {"/websocket", ws_h, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}).

loop() ->
    timer:sleep(10000),
    loop().
