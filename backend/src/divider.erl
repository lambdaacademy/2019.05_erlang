-module(divider).

-export([divide/2, divide/3, get_node/4, get_node/5]).


-callback divide(Board :: rect:t(), NumberOfPieces  :: integer()) -> [rect:t()] | indivisable.
-callback get_piece_id(X :: integer(), Y :: integer(), SideSize :: integer(), PiecesNumber :: integer()) -> integer().

divide(Board, Nodes) ->
    StrategyModule = default_strategy_module(),
    divide(StrategyModule, Board, Nodes).

get_node(_X, Y, _, _) when Y < 0 -> {error, out_of_bounds};
get_node(X, Y, Nodes, SideSize) ->
    StrategyModule = default_strategy_module(),
    get_node(StrategyModule, X, Y, Nodes, SideSize).

get_node(StrategyMod, X, Y, Nodes, SideSize) ->
    PiecesN = length(Nodes),
    case StrategyMod:get_piece_id(X, Y, SideSize, PiecesN) of
        {error, Reason} ->
            {error, Reason};
        Ind when is_integer(Ind) ->
            lists:nth(Ind, Nodes)
    end.


% returns [{node(), rect()}]
% TODO remove fields from rects here (so that slaves don't get them
divide(StrategyMod, Board, Nodes) when is_list(Nodes) ->
    NumberOfPieces = length(Nodes),
    Rects = StrategyMod:divide(Board, NumberOfPieces),
    io:format("~p, ~p", [Nodes, Rects]),
    lists:zip(Nodes, Rects).

default_strategy_module() -> stripes.
