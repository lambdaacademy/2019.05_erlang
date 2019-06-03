# ConwayGame
This is a project for 2019 edition of School of Erlang.

## Manual

- Modify master_vars.config/slave_vars.config (replace `host` with your IP address)
- Add to src/conway_game.app.src the master node name
- Default number of slaves is 1, you can change it by modifying src/conway_game.app.src 

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
