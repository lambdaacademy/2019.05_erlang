-module(rect).

-compile(export_all).

-type t() :: #{ width := non_neg_integer(),
                height := non_neg_integer(),
                fields := [cell()],
                gap := 0 }.
-type cell() :: { Coords :: point(),
                  IsAlive :: boolean() }. 
-type point() :: { X :: non_neg_integer(),
                   Y:: non_neg_integer() }. 

-spec new(W :: non_neg_integer(), H :: non_neg_integer(), Gap :: non_neg_integer()) -> t().
new(W, H, Gap) ->
    Fields = [{coords(X, Y), false} || X <- lists:seq(0, W-1), Y <- lists:seq(0, H-1)],
    new(W, H, Gap, Fields).

-spec new(W :: non_neg_integer(), H :: non_neg_integer(), Gap :: non_neg_integer(), Fields :: [cell]) -> t().
new(W, H, Gap, Fields) ->
    #{ width => W,
       height => H,
       gap => Gap,
       fields => Fields }.

get_row(#{ gap := Gap } = M, upper) ->
    get_row(M, Gap);
get_row(#{ height := H, gap := Gap } = M, lower) ->
    get_row(M, H+Gap);
get_row(#{ fields := F }, Row) ->
   lists:filter(fun({{X, _}, _}) -> X == Row end, F). 

get(#{ fields := F }, Coords) ->
    proplists:get_value(Coords, F, out_of_bounds). 

set(#{ fields := F } = M, Coords, Value) ->
    NewF = proplists:delete(Coords, F),
    M#{ fields := [{Coords, Value} | NewF] }.

map(Fun, #{ fields := F } = M) ->
    M#{ fields := lists:map(fun({{X, Y}, Value}) ->
                                    Fun(X, Y, Value) end, F)}.

cut_out(#{ fields := F }, {FromX, FromY} = From, {ToX, ToY} = To) ->
    Remaining = lists:filter(fun({Coords, _}) -> between(Coords, From, To) end, F),
    #{ fields => Remaining, gap => FromX, width => ToX - FromX + 1, height => ToY - FromY + 1 }.

fields(#{ fields := F }) ->
    F.

join([]) ->
    [];
join([#{ fields := Fields }|T]) ->
    Fields ++ join(T).

coords(X, Y) ->
    {X, Y}.

between({X, Y}, {X1, Y1}, {X2, Y2}) -> X >= X1 andalso X =< X2
                                       andalso Y >= Y1 andalso Y =< Y2;
between(X, X1, X2) -> X >= X1 andalso X =< X2.


match_coords({X, Y}, {XX, YY}) ->
    X == XX andalso Y == YY.
