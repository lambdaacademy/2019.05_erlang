-module(stripes).

-behaviour(divider).

-export([divide/2, get_piece_id/4]).

divide(#{ height := SideSize } = Board, StripesN) ->
    case StripesN > SideSize of
        true -> indivisable;
        false -> unsafe_divide(Board, StripesN)
    end.

% @doc REturn the index at which the node is in the list of nodes.
% starts indexing from 1 (accordingly to lists module).
get_piece_id(_X, Y, SideSize, StripesN) ->
    DivisionIndices = get_division_indices(SideSize, StripesN),
    get_index_of_piece(Y, DivisionIndices).

get_index_of_piece(Y, DivisionIndices) -> get_index_of_piece(Y, DivisionIndices, 1).

get_index_of_piece(Y, [NextInd | _], CurrInd) when NextInd >= Y ->
    CurrInd;
get_index_of_piece(Y, [_ | DivisionIndices], CurrInd) ->
    get_index_of_piece(Y, DivisionIndices, CurrInd + 1);
get_index_of_piece(_, _, _) ->
    {error, out_of_bounds}.

unsafe_divide(#{height := SideSize} = Rect, StripesN) ->
    Indices0 = get_division_indices(SideSize, StripesN),
    Indices = make_indices_pairs(Indices0),
    [rect:cut_out(Rect, {0, Prev + 1}, {SideSize - 1, Curr})
     || {Prev, Curr} <- Indices].


% @doc Returns a list of indexes that are the last indexes of the divided stripes.
get_division_indices(SideSize, StripesN) ->
    BaseStripeThickness = SideSize div StripesN,
    Rest = SideSize rem StripesN,
    [X * (BaseStripeThickness + 1) - 1 || X <- lists:seq(1, Rest)]
    ++
    [Rest * (BaseStripeThickness + 1) - 1 + X * BaseStripeThickness || X <- lists:seq(1, StripesN - Rest)].

make_indices_pairs(Indices) ->
    Second = Indices,
    First = lists:droplast([-1 | Indices]),
    lists:zip(First, Second).
