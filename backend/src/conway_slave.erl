-module(conway_slave).

-export([loop/0]).

loop() ->
    io:format("I am alive~n", []),
    timer:sleep(10000),
    loop().
