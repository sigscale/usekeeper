%%% usekeeper.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2019 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This library module implements the public API for the
%%% 	{@link //usekeeper. usekeeper} application.
%%%
-module(usekeeper).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

%% export the usekeeper public API
-export([add_user/2, add_user/4, list_users/0, get_user/1, delete_user/1,
		add_usage_spec/1, delete_usage_spec/1, query/6,
		add_usage/1, query_usage/5]).

%% export the usekeeper private API
-export([]).

-include("usage.hrl").
-include_lib("inets/include/mod_auth.hrl").

%%----------------------------------------------------------------------
%%  The usekeeper public API
%%----------------------------------------------------------------------

-spec add_user(Username, Password) -> Result
	when
		Username :: string(),
		Password :: string(),
		Result :: {ok, {Id, LastModified}} | {error, Reason},
		Id :: string(),
		LastModified :: {pos_integer(), pos_integer()},
		Reason :: term().
%% @equiv add_user(undefined, undefined, Username, Password)
add_user(Username, Password) when is_list(Username),
		is_list(Password) ->
	add_user(undefined, undefined, Username, Password).

-spec add_user(FirstName, LastName, Username, Password) -> Result
	when
		FirstName :: string() | undefined,
		LastName :: string() | undefined,
		Username :: string(),
		Password :: string(),
		Result :: {ok, {Id, LastModified}} | {error, Reason},
		Id :: string(),
		LastModified :: {pos_integer(), pos_integer()},
		Reason :: user_exists | term().
%% @doc Add an HTTP user.
%%
%% 	HTTP Basic authentication (RFC7617) is required with
%% 	`Username' and  `Password' used to construct the
%% 	`Authorization' header in REST requests.
%%
add_user(FirstName, LastName, Username, Password)
		when is_list(FirstName), is_list(Username), is_list(Password) ->
	add_user1(LastName, Username, Password, [{givenName, FirstName}]);
add_user(undefined, LastName, Username, Password)
		when is_list(Username), is_list(Password) ->
	add_user1(LastName, Username, Password, []).
%% @hidden
add_user1(LastName, Username, Password, UserData)
		when is_list(LastName) ->
	add_user2(Username, Password,
			[{lastName, LastName} | UserData], get_params());
add_user1(undefined, Username, Password, UserData) ->
	add_user2(Username, Password, UserData, get_params()).
%% @hidden
add_user2(Username, Password, UserData, {Port, Address, Dir, Group}) ->
	{Id, LM} = unique(),
	NewUserData = [{id, Id}, {last_modified, LM} | UserData],
	add_user3(Username, Address, Port, Dir, Group, Id, LM,
			mod_auth:add_user(Username, Password, NewUserData, Address, Port, Dir));
add_user2(_, _, _, {error, Reason}) ->
	{error, Reason}.
%% @hidden
add_user3(Username, Address, Port, Dir, Group, Id, LM, true) ->
	add_user4(Id, LM, mod_auth:add_group_member(Group, Username, Address, Port, Dir));
add_user3(_, _, _, _, _, _, _, {error, Reason}) ->
	{error, Reason}.
%% @hidden
add_user4(Id, LM, true) ->
	{ok, {Id, LM}};
add_user4(_, _, {error, Reason}) ->
	{error, Reason}.

-spec list_users() -> Result
	when
		Result :: {ok, Users} | {error, Reason},
		Users :: [Username],
		Username :: string(),
		Reason :: term().
%% @doc List HTTP users.
%% @equiv  mod_auth:list_users(Address, Port, Dir)
list_users() ->
	list_users1(get_params()).
%% @hidden
list_users1({Port, Address, Dir, _}) ->
	mod_auth:list_users(Address, Port, Dir);
list_users1({error, Reason}) ->
	{error, Reason}.

-spec get_user(Username) -> Result
	when
		Username :: string(),
		Result :: {ok, User} | {error, Reason},
		User :: #httpd_user{},
		Reason :: term().
%% @doc Get an HTTP user record.
%% @equiv mod_auth:get_user(Username, Address, Port, Dir)
get_user(Username) ->
	get_user(Username, get_params()).
%% @hidden
get_user(Username, {Port, Address, Dir, _}) ->
	mod_auth:get_user(Username, Address, Port, Dir);
get_user(_, {error, Reason}) ->
	{error, Reason}.

-spec delete_user(Username) -> Result
	when
		Username :: string(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Delete an existing HTTP user.
delete_user(Username) ->
	delete_user1(Username, get_params()).
%% @hidden
delete_user1(Username, {Port, Address, Dir, GroupName}) ->
	delete_user2(GroupName, Username, Address, Port, Dir,
			mod_auth:delete_user(Username, Address, Port, Dir));
delete_user1(_, {error, Reason}) ->
	{error, Reason}.
%% @hidden
delete_user2(GroupName, Username, Address, Port, Dir, true) ->
	delete_user3(mod_auth:delete_group_member(GroupName,
			Username, Address, Port, Dir));
delete_user2(_, _, _, _, _, {error, Reason}) ->
	{error, Reason}.
%% @hidden
delete_user3(true) ->
	ok;
delete_user3({error, Reason}) ->
	{error, Reason}.

-spec add_usage_spec(UsageSpec) -> Result
	when
		Result :: {ok, UsageSpec} | {error, Reason},
		UsageSpec :: use_spec(),
		Reason :: term().
%% @doc Create a new Usage Specification.
add_usage_spec(#use_spec{id = undefined,
		last_modified = undefined} = UsageSpec) ->
	F = fun() ->
			{Id, LM} = unique(),
			NewUsageSpec = UsageSpec#use_spec{id = Id, last_modified = LM},
			ok = mnesia:write(NewUsageSpec),
			NewUsageSpec
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, NewUsageSpec} ->
			{ok, NewUsageSpec}
	end.

-spec delete_usage_spec(UsageSpecId) -> Result
	when
		UsageSpecId :: string(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Delete an existing Usage Specification.
delete_usage_spec(UsageSpecId) when is_list(UsageSpecId) ->
	F = fun() ->
			mnesia:delete(use_spec, UsageSpecId, write)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, ok} ->
			ok
	end.

-spec query(Cont, Size, Sort, Table, MatchSpec, CountOnly) -> Result
	when
		Cont :: start | any(),
		Size :: pos_integer() | undefined,
		Table :: atom(),
		Sort :: [integer()],
		MatchSpec :: ets:match_pattern() | '_',
		CountOnly :: boolean(),
		Result :: {Cont1, [term()], Total} | {error, Reason},
		Cont1 :: eof | any(),
		Total :: non_neg_integer(),
		Reason :: term().
%% @doc Query the `Table'.
%%
%% 	Provides a generic function to query {@link //mnesia. mnesia}
%% 	tables with fine grain control over the selection and results.
%%
%% 	If `Size' is not `undefined' results will be paginated
%% 	with at most `Size' items per page. The next page is returned
%% 	by providing `Cont' as the value of `Cont1' from the result
%% 	of the previous call.
%%
%%		The result list will be sorted by the record elements listed
%% 	in `Sort', in order of appearance.
%%
%% 	Query selection is controlled by `MatchSpec'. See the
%% 	{@link //erts. erts} User's Guide for definition of
%% 	Match Specifications.
%%
%% 	If `CountOnly' is `true' the result list will be empty.
%%
query(Cont, undefined, Sort, Table, MatchSpec, CountOnly)
		when is_atom(Table), is_list(Sort), is_boolean(CountOnly) ->
	{ok, Size} = application:get_env(usekeeper, rest_page_size),
	query1(Cont, Size, Sort, Table, MatchSpec, CountOnly);
query(Cont, Size, Sort, Table, MatchSpec, CountOnly)
		when is_atom(Table), is_integer(Size),
		is_list(Sort), is_boolean(CountOnly) ->
	query1(Cont, Size, Sort, Table, MatchSpec, CountOnly).
%% @hidden
query1(start, Size, [], Table, '_', true) ->
	{eof, Size, mnesia:table_info(Table, size)};
query1(start, Size, [], Table, '_', false) ->
	MatchSpec = [{'_', [], ['$_']}],
	F = fun() ->
			{mnesia:select(Table, MatchSpec, Size, read),
					mnesia:table_info(Table, size)}
	end,
	query2(mnesia:ets(F), [], false);
query1(start, Size, [], Table, MatchSpec, false)
		when is_integer(Size) ->
	F = fun() ->
		{mnesia:select(Table, MatchSpec, Size, read), undefined}
	end,
	query2(mnesia:ets(F), [], false);
query1(start, _Size, Sort, Table, MatchSpec, CountOnly) ->
	F = fun() ->
		{mnesia:select(Table, MatchSpec, read), undefined}
	end,
	query2(mnesia:ets(F), Sort, CountOnly);
query1(Cont, _Size, Sort, Table, '_', CountOnly) ->
	F = fun() ->
		{mnesia:select(Cont), mnesia:table_info(Table, size)}
	end,
	query2(mnesia:ets(F), Sort, CountOnly);
query1(Cont, _Size, Sort, _Table, _MatchSpec, CountOnly) ->
	F = fun() ->
		{mnesia:select(Cont), undefined}
	end,
	query2(mnesia:ets(F), Sort, CountOnly).
%% @hidden
query2({Objects, undefined}, _Sort, true)
		when is_list(Objects) ->
	Total = length(Objects),
	{eof, Total, Total};
query2({Objects, undefined}, Sort, false)
		when is_list(Objects) ->
	Total = length(Objects),
	query3(eof, Objects, Total, lists:reverse(Sort));
query2({{Objects, Cont}, Total}, _Sort, true)
		when is_integer(Total) ->
	{Cont, length(Objects), Total};
query2({{Objects, Cont}, Total}, Sort, false) ->
	query3(Cont, Objects, Total, lists:reverse(Sort));
query2({'$end_of_table', _Total},
		_Sort, _CountOnly) ->
	{eof, []}.
%% @hidden
query3(Cont, Objects, Total, [H | T]) when H > 0 ->
	query3(Cont, lists:keysort(H, Objects), Total, T);
query3(Cont, Objects, Total, [H | T]) when H < 0 ->
	query3(Cont, lists:reverse(lists:keysort(-H, Objects)), Total, T);
query3(Cont, Objects, undefined, []) ->
	{Cont, Objects};
query3(Cont, Objects, Total, []) ->
	{Cont, Objects, Total}.

-spec add_usage(Usage) -> Result
	when
		Usage :: usage(),
		Result :: {ok, UsageLog} | {error, Reason},
		UsageLog :: usage_log(),
		Reason :: term().
%% @doc Log a new Usage Event.
add_usage(#{"date" := Date, "type" := Type} = Usage)
		when is_list(Date), is_list(Type) ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	ID = integer_to_list(TS) ++ integer_to_list(N),
	Href = "/usageManagement/v4/usage/" ++ ID,
	UsageLog = {TS, N, Usage#{"id" => ID, "href" => Href}},
	case disk_log:log(usage, UsageLog) of
		ok ->
			{ok, UsageLog};
		{error, Reason} ->
			{error, Reason}
	end.

-spec query_usage(Cont, Size, Sort, MatchSpec, CountOnly) -> Result
	when
		Cont :: start | any(),
		Size :: pos_integer() | undefined,
		Sort :: [integer()],
		MatchSpec :: ets:match_pattern() | '_',
		CountOnly :: boolean(),
		Result :: {Cont1, [term()]} | {Cont1, [term()], Total} | {error, Reason},
		Cont1 :: eof | any(),
		Total :: non_neg_integer() | undefined,
		Reason :: term().
%% @doc Query the `usage' log.
%%
%%    The first time called `Cont' should have the value `start'.
%%
%% 	If `Size' is not `undefined' results will be paginated
%% 	with at most `Size' items per page. The next page is returned
%% 	by providing `Cont' as the value of `Cont1' from the result
%% 	of the previous call.
%%
%%		The result list will be sorted by the keys of the usage map listed
%% 	in `Sort', in order of appearance.
%%
%% 	Query selection is controlled by `MatchSpec'. See the
%% 	{@link //erts. erts} User's Guide for definition of
%% 	Match Specifications.
%%
%%    If `CountOnly' is `true' the result list will be empty.
%%
query_usage(Cont, undefined, Sort, MatchSpec, CountOnly)
		when is_list(Sort), is_boolean(CountOnly) ->
	{ok, Size} = application:get_env(usekeeper, rest_page_size),
	query_usage1(Cont, Size, Sort, MatchSpec, CountOnly);
query_usage(Cont, Size, Sort, MatchSpec, CountOnly)
		when is_integer(Size), is_list(Sort), is_boolean(CountOnly) ->
	query_usage1(Cont, Size, Sort, MatchSpec, CountOnly).
%% @hidden
query_usage1(_Cont, Size, _Sort, '_', true) ->
	{no_items, N} = lists:keyfind(no_items, 1, disk_log:info(usage)),
	{eof, Size, N};
query_usage1(Cont, Size, Sort, '_', false) ->
	{no_items, N} = lists:keyfind(no_items, 1, disk_log:info(usage)),
	query_usage2(disk_log:chunk(usage, Cont, Size), Sort, '_', false, N);
query_usage1(Cont, Size, Sort, MatchSpec, CountOnly) ->
	query_usage2(disk_log:chunk(usage, Cont, Size),
			Sort, MatchSpec, CountOnly, undefined).
%% @hidden
query_usage2(eof, _Sort, _MatchSpec, _CountOnly, undefined) ->
	{eof, []};
query_usage2(eof, _Sort, _MatchSpec, _CountOnly, Total) ->
	{eof, [], Total};
query_usage2({error, Reason}, _Sort, _MatchSpec, _CountOnly, _Total) ->
	{error, Reason};
query_usage2({Cont, Chunk, 0}, Sort, '_', false, Total) when is_list(Chunk) ->
	query_usage4(Cont, Chunk, Total, lists:reverse(Sort));
query_usage2({Cont, Chunk, 0}, Sort, MatchSpec, CountOnly, _Total)
		when is_list(Chunk) ->
	query_usage3(Cont, Chunk, Sort, MatchSpec, CountOnly, []);
query_usage2({Cont, Chunk}, Sort, '_', false, Total) when is_list(Chunk) ->
	query_usage4(Cont, Chunk, Total, lists:reverse(Sort));
query_usage2({Cont, Chunk}, Sort, MatchSpec, CountOnly, _Total)
		when is_list(Chunk) ->
	query_usage3(Cont, Chunk, Sort, MatchSpec, CountOnly, []).
%% @hidden
query_usage3(Cont, [H | T], Sort, MatchSpec, CountOnly, Acc)
		when is_list(MatchSpec) ->
	case erlang:match_spec_test(element(3, H), MatchSpec, table) of
		{ok, #{}, [], []} ->
			query_usage3(Cont, T, Sort, MatchSpec, CountOnly, [H | Acc]);
		{ok, false , [], []}->
			query_usage3(Cont, T, Sort, MatchSpec, CountOnly, Acc);
		{error, Reason} ->
			{error, Reason}
	end;
query_usage3(Cont, [], Sort, _MatchSpec, false, Acc) ->
	query_usage4(Cont, Acc, length(Acc), lists:reverse(Sort));
query_usage3(Cont, [], _Sort, _MatchSpec, true, Acc) ->
	{Cont, [], length(Acc)}.
%% @hidden
query_usage4(Cont, Objects, Total, [H | T]) when H > 0 ->
	query_usage4(Cont, lists:keysort(H, Objects), Total, T);
query_usage4(Cont, Objects, Total, [H | T]) when H < 0 ->
	query_usage4(Cont, lists:reverse(lists:keysort(-H, Objects)), Total, T);
query_usage4(Cont, Objects, undefined, []) ->
	{Cont, Objects};
query_usage4(Cont, Objects, Total, []) ->
	{Cont, Objects, Total}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec get_params() -> Result
	when
		Result :: {Port :: integer(), Address :: string(),
		Directory :: string(), Group :: string()}
				| {error, Reason :: term()}.
%% @doc Returns configurations details for currently running
%% 	{@link //inets. httpd} service.
%% @hidden
get_params() ->
	get_params(inets:services_info()).
%% @hidden
get_params({error, Reason}) ->
	{error, Reason};
get_params(ServicesInfo) ->
	get_params1(lists:keyfind(httpd, 1, ServicesInfo)).
%% @hidden
get_params1({httpd, _, HttpdInfo}) ->
	{_, Address} = lists:keyfind(bind_address, 1, HttpdInfo),
	{_, Port} = lists:keyfind(port, 1, HttpdInfo),
	get_params2(Address, Port, application:get_env(inets, services));
get_params1(false) ->
	{error, httpd_not_started}.
%% @hidden
get_params2(Address, Port, {ok, Services}) ->
	get_params3(Address, Port, lists:keyfind(httpd, 1, Services));
get_params2(_, _, undefined) ->
	{error, inet_services_undefined}.
%% @hidden
get_params3(Address, Port, {httpd, Httpd}) ->
	get_params4(Address, Port, lists:keyfind(directory, 1, Httpd));
get_params3(_, _, false) ->
	{error, httpd_service_undefined}.
%% @hidden
get_params4(Address, Port, {directory, {Directory, Auth}}) ->
	get_params5(Address, Port, Directory,
			lists:keyfind(require_group, 1, Auth));
get_params4(_, _, false) ->
	{error, httpd_directory_undefined}.
%% @hidden
get_params5(Address, Port, Directory, {require_group, [Group | _]}) ->
	{Port, Address, Directory, Group};
get_params5(_, _, _, false) ->
	{error, httpd_group_undefined}.

-spec unique() -> Result
	when
		Result :: {ID, LM},
		ID :: string(),
		LM :: {TS, N},
		TS :: pos_integer(),
		N :: pos_integer().
%% @doc Generate a unique identifier and timestamp.
unique() ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	ID = integer_to_list(TS) ++ integer_to_list(N),
	LM = {TS, N},
	{ID, LM}.

