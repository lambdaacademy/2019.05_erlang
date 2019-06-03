-module(communication_utils).

-export([game_state_to_json/1, fields_from_maps/1]).

game_state_to_json(#{start_state := wait_for_slaves, slaves := Slaves, max_slaves := MaxSl}) ->
    jiffy:encode(#{<<"start_state">> => <<"wait_for_slaves">>,
                   <<"slaves">> => Slaves,
                   <<"max_slaves">> => MaxSl
                  });
game_state_to_json(#{start_state := wait_for_start_computation, slaves := Slaves, max_slaves := MaxSl}) ->
    jiffy:encode(#{
      <<"start_state">> => <<"wait_for_start">>,
      <<"slaves">> => Slaves,
      <<"max_slaves">> => MaxSl
     });
game_state_to_json(#{start_state := computing, board := Board, generation := G, goal_generations := GG, slaves := Slaves, max_slaves := MaxSl}) ->
    Fields = fields_to_maps(rect:fields(Board)),
    #{ width := Size } = Board,
    jiffy:encode(
      #{<<"fields">> => Fields,
        <<"size">> => Size,
        <<"start_state">> => <<"computing">>,
        <<"generation">> => G,
        <<"goalGeneration">> => GG,
        <<"slaves">> => Slaves,
        <<"max_slaves">> => MaxSl
       }).

fields_from_maps(Fields) ->
    [{{maps:get(<<"x">>, Field), maps:get(<<"y">>, Field)}, maps:get(<<"alive">>, Field)}
     || Field <- Fields].

fields_to_maps(Fields) ->
    [#{<<"x">> => X, <<"y">> => Y, <<"alive">> => Alive} || {{X, Y}, Alive} <- Fields].
