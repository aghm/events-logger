-module(user_handler).

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
	[_AccountId, ClientSessId, ClientUserId] = [proplists:get_value(K, FormData) || 
				K <- [<<"accountId">>, <<"sessionId">>, <<"userId">>]
			],
	SomeAbsent = lists:member(undefined, [_AccountId, ClientSessId, ClientUserId]),
	case SomeAbsent of
		true  -> cowboy_req:reply(400, [], <<"accountId, sessionId and userId must be provided">>, Req);
		false -> 
			{AccountId, _} = string:to_integer(erlang:binary_to_list(_AccountId)),
			{SessionStatus, _, {Session}} = pgservice:get_or_create_session(self(), AccountId, ClientSessId),
			{UserStatus, _, {User}} = pgservice:get_or_create_user(self(), AccountId, ClientUserId),
			case [SessionStatus, UserStatus] of
				[error, _] -> cowboy_req:reply(400, [], <<"Invalid session or account Id">>, Req);
				[_, error] -> cowboy_req:reply(500, [], <<"Unable to create or mutate user. Contact support.">>, Req);
				[ok, ok] -> 
					MergeStatus = pgservice:merge_session(self(), Session, User),
					case MergeStatus of
						ok -> cowboy_req:reply(200, Req);
						error -> cowboy_req:reply(500, [], <<"Unable to merge profile. Contact support.">>, Req)
					end
			end
	end.