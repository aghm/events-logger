-module(warehouse).

-export([start/0]).

start() ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowlib),
	ok = application:start(cowboy),
	application:ensure_all_started(pgapp),
	ok = application:start(warehouse).