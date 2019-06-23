-module(conway_master_worker).

-behaviour(gen_server).

%% API
-export([noderef/0]).

-export([start_link/1,
         run_for_board/2,
         get_state/0,
         get_state_raw/0,
         reset_state/0,
         register_slave/1,
         update_state/3]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-record(state, {started = false,
                board = null,
                generation = 1,
                goal_generations = null,
                state = wait_for_slaves,
                slaves = [],
                max_slaves,
                current_rects = #{}}).

start_link(SlaveN) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [SlaveN], []).

run_for_board(Board, GoalGenerations) ->
    gen_server:call(noderef(), {run, Board, GoalGenerations}).

register_slave(NodeName) ->
    gen_server:call(noderef(), {hello, NodeName}).

update_state(NodeName, Rect, Generation) ->
    gen_server:cast(noderef(), {update, NodeName, Rect, Generation}).

get_state() -> gen_server:call(noderef(), get_state).

reset_state() -> gen_server:call(noderef(), reset_state).

get_state_raw() -> gen_server:call(noderef(), get_state_raw).

init([SlaveN]) ->
    io:format("Master node ~p~n", [node()]),
    {ok, #state{max_slaves = SlaveN, started = true}}.

handle_call(reset_state, _, #state{ max_slaves = MaxSl }) ->
    Fresh = #state{ max_slaves = MaxSl, started = true },
    {reply, format_state(Fresh), Fresh};
handle_call({hello, NodeName}, _, #state{state = wait_for_slaves, max_slaves = MaxSl} = State) ->
    io:format("Registering ~p~n", [NodeName]),
    NewSlaves = add_slave(NodeName, State#state.slaves),
    case NewSlaves of
        {error, _} = E ->
            {reply, E, State};
        E when length(E) == MaxSl ->
            NewState = State#state{slaves = NewSlaves, state = wait_for_start_computation},
            monitor_node(NodeName, true),
            ws_h:update_state(format_state(NewState)),
            {reply, ok, NewState};
        _ ->
            NewState = State#state{slaves = NewSlaves},
            ws_h:update_state(format_state(NewState)),
            {reply, ok, NewState}
    end;
handle_call({run, Board, Gens}, _From, #state{state = wait_for_start_computation,
                                              slaves = Slaves} = State) ->
%%    io:format("Board:~n~s~nGoal generations = ~p~n",
%%              [Board, Gens]),

    divide_and_send(Board, Slaves, Gens),
    {reply, ok, State#state{started = true,
                            board = Board,
                            goal_generations = Gens,
                            state = computing}};
handle_call(get_state, _From, State) ->
    Resp = format_state(State),
    {reply, Resp, State};
handle_call(get_state_raw, _From, State) ->
    {reply, State, State};
handle_call(M, _, S) ->
    io:format("[WARN] Unknown call: ~p in state ~p~n", [M, S]),
    {reply, unknown, S}.


handle_cast({update, NodeName, Rect, G}, #state{state = computing,
                                                generation = G,
                                                current_rects = R,
                                                max_slaves = SlaveN} = State) ->
    NewCurrentRects = add_to_current_rects(NodeName, Rect, R, State#state.slaves),
    case maps:size(NewCurrentRects) of
        SlaveN ->
            NewBoard = update_board(State#state.board, NewCurrentRects),
            NewState = State#state{board = NewBoard},
            inc_generation(NewState#state{current_rects = #{}});
        _ ->
            {noreply, State#state{current_rects = NewCurrentRects}}
    end;
handle_cast(M, S) ->
    io:format("[WARN] Unknown cast: ~p ~n", [M]),
    io:format("[WARN] in state ~p~n", [S]),
    io:format("[WARN] State: ~p ~n", [S#state.state]),
    {noreply, S}.

handle_info({'DOWN', Ref, _, _, Reason} = M, S) ->
    io:format("Got down message from ~p: ~p~nReason: ~p~n", [Ref, M, Reason]),
    {noreply, S};
handle_info(_, S) ->
    {noreply, S}.

%% Internal functions

noderef() ->
    {ok, NodeName} = application:get_env(conway_game, masterhost),
    {?MODULE, NodeName}.

inc_generation(#state{goal_generations = N, generation = M} = State) when N == M ->
    NewState = State#state{state = finished, generation = M},
    ws_h:update_state(format_state(NewState)),
    io:format("Incrementing generation to ~p~n", [NewState#state.generation]),
    {noreply, NewState};
inc_generation(State) ->
    NewState = State#state{generation = State#state.generation + 1},
    io:format("Incrementing generation to ~p~n", [NewState#state.generation]),
    ws_h:update_state(format_state(NewState)),
    {noreply, NewState}.

format_state(#state{state = S, board = B, generation = G, goal_generations = GG, slaves = Sl, max_slaves = MaxSl}) ->
    #{start_state => S,
      board => B,
      generation => G,
      goal_generations => GG,
      slaves => Sl,
      max_slaves => MaxSl}.

divide_and_send(#{ width := W, height := H} = Board, Slaves, Gens) when W == H ->
    Divided = divider:divide(Board, Slaves),
    [conway_slave:init_state(Node, Part, Gens, Slaves, W) || {Node, Part} <- Divided].


add_slave(Slave, Slaves) ->
    case lists:member(Slave, Slaves) of
        true ->
            {error, already_registered};
        _ ->
            [Slave | Slaves]
    end.

add_to_current_rects(NodeName, Rect, R, Slaves) ->
    case lists:member(NodeName, Slaves) of
        true ->
            maps:put(NodeName, Rect, R);
        false ->
            io:format("[ERROR] Trying to add rect for unknown slave"),
            R
    end.

% TODO Actually we only need a list of fields here... What do we need board module/struct for?
update_board(#{ width := W, height := H }, Rects) ->
    NewFields = rect:join(maps:values(Rects)),
    rect:new(W, H, 0, NewFields).
