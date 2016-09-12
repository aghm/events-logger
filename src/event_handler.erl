-module(event_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(TAG_MAX_LENGTH, 32).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	{ok, Req2} = respond(Method, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

respond(<<"GET">>, Req) ->
	cowboy_req:reply(405, [], <<"Method not supported">>, Req);
respond(<<"POST">>, Req) ->
	{ok, FormData, _} = cowboy_req:body_qs(Req),
	[_AccountId, ClientSessId, EventTag, Namespace, ExtraData] = [
		proplists:get_value(K, FormData) || 
		K <- [<<"accountId">>, <<"sessionId">>, <<"event">>, <<"namespace">>, <<"context">>] 
	],
	TagLenExceeds = (string:len(erlang:binary_to_list(EventTag)) > ?TAG_MAX_LENGTH),
	NSLenExceeds = (namespace_length(Namespace) > ?TAG_MAX_LENGTH),
	SomeAbsent = lists:member(undefined, [_AccountId, ClientSessId, EventTag]),
	case [SomeAbsent, NSLenExceeds, TagLenExceeds] of
		[true, _, _] -> cowboy_req:reply(400, [], <<"accountId, sessionId and event must be present">>, Req);
		[_, true, _] -> cowboy_req:reply(400, [], <<"namespace must be within 32 characters">>, Req);
		[_, _, true] -> cowboy_req:reply(400, [], <<"event must be within 32 characters">>, Req);
		[false, false, false] ->
			{AccountId, _} = string:to_integer(erlang:binary_to_list(_AccountId)),
			{Status, _, {RowId}} = pgservice:get_or_create_session(self(), AccountId, ClientSessId),
			case Status of
				error -> 
					cowboy_req:reply(400, [], <<"Invalid session or account Id">>, Req);
				ok ->
					pgservice:store_event(self(), EventTag, Namespace, ExtraData, RowId),
					cowboy_req:reply(200, Req)
			end
	end.

namespace_length(NS) ->
	case NS of
		undefined -> 0;
		_ -> string:len(erlang:binary_to_list(NS))
	end.