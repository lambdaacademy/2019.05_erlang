# ConwayGame
This is a project for 2019 edition of School of Erlang.

## Manual

- Modify master_vars.config/slave_vars.config (replace `host` with your IP address)

The following configuration parameters must be specified in `src/conway_game.app.src`
- `masternode` - master node name (replace `host` with your IP address);
- `slaves_n` - default is 1. 

### Master
```
./rebar3 as master release

_build/master/rel/conway_game/bin/conway_game console
```

### Slave
```
./rebar3 as slave release

_build/slave/rel/conway_game/bin/conway_game console
```

#### Adding more slaves

Each slave should have a unique node name (`slave_vars.config` needs to be modified accordingly).
