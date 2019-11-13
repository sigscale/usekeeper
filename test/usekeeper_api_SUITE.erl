%%% usekeeper_api_SUITE.erl
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
%%% Test suite for the public API of the
%%% {@link //usekeeper. usekeeper} application.
%%%
-module(usekeeper_api_SUITE).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

-include("usage.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("inets/include/mod_auth.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	ok = usekeeper_test_lib:initialize_db(),
	ok = usekeeper_test_lib:start(),
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	ok = usekeeper_test_lib:stop().

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test case.
%%
init_per_testcase(_TestCase, Config) ->
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, _Config) ->
	ok.

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() ->
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[add_user, get_user, list_users, delete_user,
			add_usage_spec, delete_usage_spec].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

add_user() ->
	[{userdata, [{doc, "Create a new HTTP user"}]}].

add_user(_Config) ->
	User = random_string(),
	Password = random_string(),
	{ok, {TS, N} = LastModified} = usekeeper:add_user(User, Password),
	true = is_integer(TS),
	true = is_integer(N),
	{Port, Address, Dir, _} = get_params(),
	{ok, #httpd_user{username = User, password = Password,
			user_data = UserData}} = mod_auth:get_user(User, Address, Port, Dir),
	{_, LastModified} = lists:keyfind(last_modified, 1, UserData).

get_user() ->
	[{userdata, [{doc, "Look up an HTTP user"}]}].

get_user(_Config) ->
	User = random_string(),
	Password = random_string(),
	{ok, {TS, N} = LastModified} = usekeeper:add_user(User, Password),
	true = is_integer(TS),
	true = is_integer(N),
	{ok, #httpd_user{username = User, password = Password,
			user_data = UserData}} = usekeeper:get_user(User),
	{_, LastModified} = lists:keyfind(last_modified, 1, UserData).

list_users() ->
	[{userdata, [{doc, "List all HTTP users"}]}].

list_users(_Config) ->
	User1 = random_string(),
	{ok, _} = usekeeper:add_user(User1, random_string()),
	User2 = random_string(),
	{ok, _} = usekeeper:add_user(User2, random_string()),
	User3 = random_string(),
	{ok, _} = usekeeper:add_user(User3, random_string()),
	{ok, Users} =  usekeeper:list_users(),
	true = lists:all(fun erlang:is_list/1, Users),
	true = lists:member(User1, Users),
	true = lists:member(User2, Users),
	true = lists:member(User3, Users).

delete_user() ->
	[{userdata, [{doc, "Remove an HTTP user"}]}].

delete_user(_Config) ->
	User = random_string(),
	Password = random_string(),
	{ok, _} = usekeeper:add_user(User, Password),
	{ok, _} = usekeeper:get_user(User),
	ok = usekeeper:delete_user(User),
	{error, no_such_user} = usekeeper:get_user(User).

add_usage_spec() ->
	[{userdata, [{doc, "Create a new usage specification"}]}].

add_usage_spec(_Config) ->
	Description = random_string(),
	BaseType = random_string(),
	UsageSpec = #use_spec{description = Description, base_type = BaseType},
	{ok, #use_spec{description = Description, base_type = BaseType, id = Id,
			last_modified = {TS, N}}} = usekeeper:add_usage_spec(UsageSpec),
	true = is_integer(TS),
	true = is_integer(N),
	true = is_list(Id).

delete_usage_spec() ->
	[{userdata, [{doc, "Delete a specific usage specification"}]}].

delete_usage_spec(_Config) ->
	Description = random_string(),
	BaseType = random_string(),
	UsageSpec = #use_spec{description = Description, base_type = BaseType},
	{ok, #use_spec{id = Id}} = usekeeper:add_usage_spec(UsageSpec),
	ok = usekeeper:delete_usage_spec(Id),
	F = fun() ->
			mnesia:read(use_spec, Id, read)
	end,
	{error, not_found} = case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, []} ->
			{error, not_found}
	end.

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

-spec random_string() -> String :: string().
%% @doc Generate a random string.
%% @private
random_string() ->
	random_string(rand:uniform(16) + 4).

-spec random_string(Length :: pos_integer()) -> String :: string().
%% @doc Generate a random string.
%% @private
random_string(Length) ->
	random_string(Length, []).
%% @hidden
random_string(0, Acc) ->
	Acc;
random_string(N, Acc) ->
	random_string(N - 1, [rand:uniform(95) + 31 | Acc]).

%% @hidden
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

