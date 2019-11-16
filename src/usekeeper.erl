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
-export([add_user/2, list_users/0, get_user/1, delete_user/1,
		query_users/4, query_users/5, add_usage_spec/1, delete_usage_spec/1]).

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
		Result :: {ok, LastModified} | {error, Reason},
		LastModified :: {integer(), integer()},
		Reason :: user_exists | term().
%% @doc Add an HTTP user.
%%
%% 	HTTP Basic authentication (RFC7617) is required with
%% 	`Username' and  `Password' used to construct the
%% 	`Authorization' header in requests.
%%
add_user(Username, Password) when is_list(Username),
		is_list(Password) ->
	add_user1(Username, Password, get_params()).
%% @hidden
add_user1(Username, Password, {Port, Address, Dir, Group}) ->
	add_user2(Username, Password,
			Address, Port, Dir, Group, usekeeper:get_user(Username));
add_user1(_, _, {error, Reason}) ->
	{error, Reason}.
%% @hidden
add_user2(Username, Password,
		Address, Port, Dir, Group, {error, no_such_user}) ->
	LM = {erlang:system_time(millisecond), erlang:unique_integer([positive])},
	NewUserData = [{last_modified, LM}],
	add_user3(Username, Address, Port, Dir, Group, LM,
			mod_auth:add_user(Username, Password, NewUserData, Address, Port, Dir));
add_user2(_, _, _, _, _, _, {error, Reason}) ->
	{error, Reason};
add_user2(_, _, _, _, _, _, {ok, _}) ->
	{error, user_exists}.
%% @hidden
add_user3(Username, Address, Port, Dir, Group, LM, true) ->
	add_user4(LM, mod_auth:add_group_member(Group, Username, Address, Port, Dir));
add_user3(_, _, _, _, _, _, {error, Reason}) ->
	{error, Reason}.
%% @hidden
add_user4(LM, true) ->
	{ok, LM};
add_user4(_, {error, Reason}) ->
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

-spec query_users(Cont, Size, Sort, MatchSpec) -> Result
	when
		Cont :: start | any(),
		Size :: pos_integer() | undefined,
		Sort :: [integer()] | [],
		MatchSpec :: ets:match_spec() | '_',
		Result :: {Cont1, [term()], Total} | {error, Reason},
		Cont1 :: eof | any(),
		Total :: non_neg_integer(),
		Reason :: term().
%% @doc Query the user table.
query_users(Cont, Size, Sort, MatchSpec) ->
	query_users(Cont, Size, Sort, MatchSpec, false).

-spec query_users(Cont, Size, Sort, MatchSpec, CountOnly) -> Result
	when
		Cont :: start | any(),
		Size :: pos_integer() | undefined,
		Sort :: [integer()] | [],
		MatchSpec :: ets:match_pattern() | '_',
		CountOnly :: boolean(),
		Result :: {Cont1, [term()], Total} | {error, Reason},
		Cont1 :: eof | any(),
		Total :: non_neg_integer(),
		Reason :: term().
%% @doc Query the user table.
%%
%%    The result list will be sorted by the record elements listed in `Sort', in order.
query_users(Cont, undefined, Sort, MatchSpec, CountOnly) ->
	{ok, Size} = application:get_env(usekeeper, rest_page_size),
	query_users1(Cont, Size, Sort, MatchSpec, CountOnly);
query_users(Cont, Size, Sort, MatchSpec, CountOnly) ->
	query_users1(Cont, Size, Sort, MatchSpec, CountOnly).
%% @hidden
query_users1(start, Size, [], '_', true) ->
	{eof, Size, mnesia:table_info(httpd_user, size)};
query_users1(start, Size, [], '_', false) ->
	MatchSpec = [{#httpd_user{_ = '_'}, [], ['$_']}],
	F = fun() ->
		{mnesia:select(httpd_user, MatchSpec, Size, read),
			mnesia:table_info(httpd_user, size)}
	end,
	query_users2(mnesia:ets(F), [], false);
query_users1(start, Size, [], MatchSpec, false)
		when is_integer(Size) ->
	F = fun() ->
		{mnesia:select(httpd_user, MatchSpec, Size, read), undefined}
	end,
	query_users2(mnesia:ets(F), [], false);
query_users1(start, _Size, Sort, MatchSpec, CountOnly) ->
	F = fun() ->
		{mnesia:select(httpd_user, MatchSpec, read), undefined}
	end,
	query_users2(mnesia:ets(F), Sort, CountOnly);
query_users1(Cont, _Size, Sort, '_', CountOnly) ->
	F = fun() ->
		{mnesia:select(Cont), mnesia:table_info(httpd_user, size)}
	end,
	query_users2(mnesia:ets(F), Sort, CountOnly);
query_users1(Cont, _Size, Sort, _MatchSpec, CountOnly) ->
	F = fun() ->
		{mnesia:select(Cont), undefined}
	end,
	query_users2(mnesia:ets(F), Sort, CountOnly).

%% @hidden
query_users2({Users, undefined}, _Sort, true)
		when is_list(Users) ->
	Total = length(Users),
	{eof, Total, Total};
query_users2({Users, undefined}, Sort, false)
		when is_list(Users) ->
	Total = length(Users),
	query_users3(eof, Users, Total, lists:reverse(Sort));
query_users2({{Users, Cont}, Total}, _Sort, true)
		when is_integer(Total) ->
	{Cont, length(Users), Total};
query_users2({{Users, Cont}, Total}, Sort, false) ->
	query_users3(Cont, Users, Total, lists:reverse(Sort));
query_users2({'$end_of_table', _Total},
		_Sort, _CountOnly) ->
	{eof, []}.
%% @hidden
query_users3(Cont, Users, Total, [H | T]) when H > 0 ->
	query_users3(Cont, lists:keysort(H, Users), Total, T);
query_users3(Cont, Users, Total, [H | T]) when H < 0 ->
	query_users3(Cont, lists:reverse(lists:keysort(-H, Users)), Total, T);
query_users3(Cont, Users, undefined, []) ->
	{Cont, Users};
query_users3(Cont, Users, Total, []) ->
	{Cont, Users, Total}.

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

