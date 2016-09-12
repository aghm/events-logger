-module(warehouse_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-define(C_ACCEPTORS,  100).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	pgapp:connect([
		{size,     environ("PG_POOL_SIZE", db_pool)},
		{host,     environ("PG_HOST", db_host)},
		{database, environ("PG_NAME", db_name)},
		{username, environ("PG_USER", db_user)},
		{password, environ("PG_USER", db_pass)}
	]),
	Routes    = routes(),
	Dispatch  = cowboy_router:compile(Routes),
	Port      = environ("PORT", http_port),
	TransOpts = [{port, Port}],
	ProtoOpts = [{env, [{dispatch, Dispatch}]}],
	{ok, _}   = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
	warehouse_sup:start_link().

stop(_State) ->
	ok.

routes() ->
	[
		{'_', [
			{"/event", event_handler, []},
			{"/merge", user_handler, []}
		]}
	].

environ(Envvar, DefAtom) ->
	case os:getenv(Envvar) of
		false ->
			{ok, Val} = application:get_env(DefAtom),
			Val;
		Other ->
			list_to_integer(Other)
	end.