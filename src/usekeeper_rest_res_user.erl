%%% usekeeper_rest_res_user.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2019 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%	  http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This library module implements resource handling functions
%%%	for a REST server in the {@link //usekeeper. usekeeper} application.
%%%
-module(usekeeper_rest_res_user).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0, get_params/0,
		get_user/2, get_users/3, post_user/1, delete_user/1, user/1]).

-include_lib("inets/include/mod_auth.hrl").
-include("usage.hrl").

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/json-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json"].

-spec delete_user(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()} .
%% @doc Respond to `DELETE /party/v4/individual/{id}' request and deletes
%% a usekeeper user. If the deletion is succeeded return true.
delete_user(Id) ->
	case usekeeper:delete_user(Id) of
		ok ->
			{ok, [], []};
		{error, _Reason} ->
			{error, 400}
	end.

-spec post_user(RequestBody) -> Result
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Handle `POST' request on `Individual' collection.
post_user(RequestBody) ->
	try
		{ok, UserMap} = zj:decode(RequestBody),
		User = user(UserMap),
		{Username, _, _, _} = User#httpd_user.username,
		Password = User#httpd_user.password,
		FirstName = case lists:keyfind(givenName, 1, User#httpd_user.user_data) of
			{_, Giv} ->
				Giv;
			false ->
				'_'
		end,
		LastName = case lists:keyfind(lastName, 1, User#httpd_user.user_data) of
			{_, Last} ->
				Last;
			false ->
				'_'
		end,
		case usekeeper:add_user(FirstName, LastName, Username, Password) of
			{ok, {_, LastModified}} ->
				User1 = user(User),
				Body = zj:encode(User1),
				Location = "/party/v4/individual" ++ Username,
				Headers = [{location, Location}, {etag, usekeeper_rest:etag(LastModified)}],
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

-spec get_users(Method, Query, Headers) -> Result
	when
		Method :: string(), % "GET" | "HEAD",
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			   | {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /partyManagement/v1/individual'
%% requests.
get_users(Method, Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_users1(Method, NewQuery, Filters, Headers);
		false ->
			get_users1(Method, Query, [], Headers)
	end.
%% @hidden
get_users1(Method, Query, Filters, Headers) ->
	case {lists:keyfind("if-match", 1, Headers),
			lists:keyfind("if-range", 1, Headers),
			lists:keyfind("range", 1, Headers)} of
		{{"if-match", Etag}, false, {"range", Range}} ->
			case global:whereis_name(Etag) of
				undefined ->
					{error, 412};
				PageServer ->
					case usekeeper_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_page(PageServer, Etag, Query, Filters, Start, End)
					end
			end;
		{{"if-match", Etag}, false, false} ->
			case global:whereis_name(Etag) of
				undefined ->
					{error, 412};
				PageServer ->
					query_page(PageServer, Etag, Query, Filters, undefined, undefined)
			end;
		{false, {"if-range", Etag}, {"range", Range}} ->
			case global:whereis_name(Etag) of
				undefined ->
					case usekeeper_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_start(Method, Query, Filters, Start, End)
					end;
				PageServer ->
					case usekeeper_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_page(PageServer, Etag, Query, Filters, Start, End)
					end
			end;
		{{"if-match", _}, {"if-range", _}, _} ->
			{error, 400};
		{_, {"if-range", _}, false} ->
			{error, 400};
		{false, false, {"range", Range}} ->
			case usekeeper_rest:range(Range) of
				{error, _} ->
					{error, 400};
				{ok, {Start, End}} ->
					query_start(Method, Query, Filters, Start, End)
			end;
		{false, false, false} ->
			query_start(Method, Query, Filters, undefined, undefined)
	end.

-spec get_user(Id, Query) -> Result
	when
		Id :: string(),
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /party/v4/individual/{id}'
%% requests.
get_user(Id, Query) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, L}, NewQuery} ->
			get_user(Id, NewQuery, string:tokens(L, ","));
		false ->
			get_user(Id, Query, [])
	end.
%% @hidden
get_user(Id, [] = _Query, _Filters) ->
	case usekeeper:get_user(Id) of
		{ok, #httpd_user{user_data = UserData} = UserRec} ->
			UserMap = user(UserRec),
			Chars = maps:get("characteristic", UserMap),
			F = fun(#{"name" := "password"}) ->
					false;
				(_) ->
					true
			end,
			NewChars = lists:filter(F, Chars),
			NewUser = UserMap#{"characteristic" => NewChars},
			Headers1 = case lists:keyfind(last_modified, 1, UserData) of
		{_, LastModified} ->
			[{etag, usekeeper_rest:etag(LastModified)}];
				false ->
					[]
			end,
			Headers2 = [{content_type, "application/json"} | Headers1],
			Body = zj:encode(NewUser),
			{ok, Headers2, Body};
		{error, _Reason} ->
			{error, 404}
	end;
get_user(_, _, _) ->
	{error, 400}.

-spec user(User) -> User
	when
		User :: #httpd_user{} | map().
%% @doc CODEC for HTTP server users.
user(#httpd_user{username = {ID, _, _, _}} = HttpdUser) ->
	user(HttpdUser#httpd_user{username  = ID});
user(#httpd_user{username = ID, password = Password, user_data = Chars})
		when is_list(ID), is_list(Chars) ->
	C = [#{"name" => "username", "value" => ID},
			#{"name" => "password", "value" => Password}],
	C1 = case lists:keyfind(givenName, 1, Chars) of
		{_, GivenName} ->
			[#{"name" => "givenName", "value" => GivenName} | C];
		false ->
			[]
	end,
	C2 = case lists:keyfind(lastName, 1, Chars) of
		{_, LastName} ->
			[#{"name" => "lastName", "value" => LastName} | C1];
		false ->
			C1
	end,
	#{"id" => ID,
		"href" => "/party/v4/individual/" ++ ID,
		"characteristic" => C2};
user(#{"id" := ID, "characteristic" := Chars})
		when is_list(ID), is_list(Chars) ->
	{Port, Address, Directory, _Group} = get_params(),
	Username = {ID, Address, Port, Directory},
	user1(Chars, #httpd_user{username = Username, user_data = Chars}).
%% @hidden
user1([#{"name" := "username", "value" := Username} | T], Acc)
		when is_list(Username) ->
	{Port, Address, Directory, _Group} = get_params(),
	user1(T, Acc#httpd_user{username = {Username, Address, Port, Directory}});
user1([#{"name" := "password", "value" := Password} | T], Acc)
		when is_list(Password) ->
	user1(T, Acc#httpd_user{password = Password});
user1([#{"name" := "givenName", "value" := GivenName} | T],
	#httpd_user{user_data = Data} = Acc) when is_list(GivenName) ->
	user1(T, Acc#httpd_user{user_data = [{givenName, GivenName} | Data]});
user1([#{"name" := "lastName", "value" := LastName} | T],
	#httpd_user{user_data = Data} = Acc) when is_list(LastName) ->
	user1(T, Acc#httpd_user{user_data = [{lastName, LastName} | Data]});
user1([_H | T], Acc) ->
	user1(T, Acc);
user1([], Acc) ->
	Acc.

-spec get_params() -> Result
	when
		Result :: {Port, Address, Directory, Group},
		Port :: integer(),
		Address :: string(),
		Directory :: string(),
		Group :: string().
%% @doc Get {@link //inets/httpd. httpd} configuration parameters.
get_params() ->
	{_, _, Info} = lists:keyfind(httpd, 1, inets:services_info()),
	{_, Port} = lists:keyfind(port, 1, Info),
	{_, Address} = lists:keyfind(bind_address, 1, Info),
	{ok, EnvObj} = application:get_env(inets, services),
	{httpd, HttpdObj} = lists:keyfind(httpd, 1, EnvObj),
	{directory, {Directory, AuthObj}} = lists:keyfind(directory, 1, HttpdObj),
	case lists:keyfind(require_group, 1, AuthObj) of
		{require_group, [Group | _T]} ->
			{Port, Address, Directory, Group};
		false ->
			exit(not_found)
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
query_start(Method, Query, Filters, RangeStart, RangeEnd) ->
	try
		CountOnly = case Method of
			"GET" ->
				false;
			"HEAD" ->
				true
		end,
		case lists:keyfind("filter", 1, Query) of
			false when length(Query) > 0 ->
				Rest = match(Query, []),
				Rest1 = [{array,[{complex, Rest}]}];
			false ->
				'_'
		end,
		MFA = [usekeeper, query, [httpd_user, '_'] ++ [CountOnly]],
		case supervisor:start_child(usekeeper_rest_pagination_sup, [MFA]) of
			{ok, PageServer, Etag} ->
				query_page(PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
			{error, _Reason} ->
				{error, 500}
		end
	catch
		_:_ ->
			{error, 400}
	end.

%% @hidden
query_page(PageServer, Etag, _Query, _Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}, infinity) of
		{error, Status} ->
			{error, Status};
		{Events, ContentRange} ->
			Users = lists:map(fun user/1, Events),
			Body = zj:encode(Users),
			Headers = [{content_type, "application/json"},
				{etag, Etag}, {accept_ranges, "items"},
				{content_range, ContentRange}],
			{ok, Headers, Body}
	end.

%% @hidden
match([{Key, Value} | T], Acc) ->
	match(T, [{exact, Key, Value} | Acc]);
match([], Acc) ->
	Acc.
