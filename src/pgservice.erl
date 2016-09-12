-module(pgservice).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_or_create_session/3, get_or_create_user/3, merge_session/3, store_event/5]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok, []}.

get_or_create_session(Pid, AccId, ClientId) ->
	gen_server:call(?MODULE, {session, AccId, ClientId}).

get_or_create_user(Pid, AccId, ClientId) ->
	gen_server:call(?MODULE, {user, AccId, ClientId}).

merge_session(Pid, SessId, UserId) ->
	gen_server:call(?MODULE, {merge, SessId, UserId}).

store_event(Pid, Tag, Namespace, Data, SessionId) ->
	gen_server:cast(?MODULE, {store, Tag, Namespace, Data, SessionId}).

handle_call({session, AccId, ClientId}, _From, _) ->
	get_or_create("sessions", AccId, ClientId);
handle_call({user, AccId, ClientId}, _From, _) ->
	get_or_create("users", AccId, ClientId);
handle_call({merge, SessId, UserId}, From, _) ->
	Result = pgapp:equery(
		"UPDATE sessions SET user_id = $1 WHERE id = $2",
		[UserId, SessId]),
	case Result of
		{ok, _} -> {reply, ok, []};
		{error, ErrMsg} -> 
			io:format("Error: ~p~n", [ErrMsg]),
			{reply, error, []}
	end.

handle_cast({store, Tag, Namespace, Data, SessionId}, State) ->
	Query = "INSERT INTO events(tag, namespace, data, session_id) VALUES($1, $2, $3, $4);",
	pgapp:equery(Query, [Tag, Namespace, Data, SessionId]),
	{noreply, State}.

handle_info(Msg, State) ->
	{noreply, State}.

terminate(normal, State) ->
	ok.

code_change(Old, State, _Extra) ->
	{ok, state}.

get_or_create(Table, AccId, ClientId) ->
	Query = query_for_table(Table, "SELECT id FROM ~s WHERE account_id = $1 AND client_id = $2;"),
	Result = pgapp:equery(Query, [AccId, ClientId]),
	case Result of
		{ok, _, Rows} ->
			case Rows of
				[] -> create_record(Table, AccId, ClientId);
				[H|_] -> {reply, {ok, false, H}, []}
			end;
		{error, ErrMsg} ->
			io:format("Error: ~p~n", [ErrMsg]),
			{reply, {error, false, []}, []}
	end.

create_record(Table, AccId, ClientId) ->
	Query = query_for_table(Table, "INSERT INTO ~s(account_id, client_id) VALUES($1, $2) RETURNING (id);"),
	Result = pgapp:equery(Query, [AccId, ClientId]),
	case Result of
		{ok, _, _, CreateRow} ->
			[H|_] = CreateRow,
			{reply, {ok, true, H}, []};
		{error, ErrMsg} ->
			io:format("Error: ~p~n", [ErrMsg]),
			{reply, {error, false, []}, []}
	end.	

query_for_table(Table, Query) ->
	Q1 = io_lib:format(Query, [Table]),
	lists:flatten(Q1).