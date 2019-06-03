# ConwayGame
This is a project for 2019 edition of School of Erlang.

## Manual
To compile and run master with tab of size N x N that will wait for two slaves before starting computation.
`./rebar3 as master release`
`SIZE=500 SLAVE_N=2 _build/master/rel/conway_game/bin/conway_game ...`
