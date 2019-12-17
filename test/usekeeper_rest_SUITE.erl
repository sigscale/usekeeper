%%% usekeeper_rest_SUITE.erl
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
%%% Test suite for the REST API of the
%%% {@link //usekeeper. usekeeper} application.
%%%
-module(usekeeper_rest_SUITE).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% common_test test cases
-export([]).

-compile(export_all).

-include("usage.hrl").
-include_lib("common_test/include/ct.hrl").

-define(PathUsage, "/usageManagement/v4/").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{timetrap, {minutes, 1}},
	{require, rest_user}, {default_config, rest_user, "ct"},
	{require, rest_pass}, {default_config, rest_pass, "tag0bpp53wsf"}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	PrivDir = ?config(priv_dir, Config),
	ok = application:set_env(mnesia, dir, PrivDir),
	ok = usekeeper_test_lib:initialize_db(),
	ok = usekeeper_test_lib:start(),
	{ok, Services} = application:get_env(inets, services),
	Fport = fun FPort([{httpd, L} | T]) ->
				case lists:keyfind(server_name, 1, L) of
					{server_name, "ct.usekeeper.org"} ->
						H1 = lists:keyfind(bind_address, 1, L),
						P1 = lists:keyfind(port, 1, L),
						{H1, P1};
					_ ->
						FPort(T)
				end;
			FPort([_ | T]) ->
				FPort(T)
	end,
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	{Host, Port} = case Fport(Services) of
		{{_, H2}, {_, P2}} when H2 == "localhost"; H2 == {127,0,0,1} ->
			{ok, _} = usekeeper:add_user(RestUser, RestPass),
			{"localhost", P2};
		{{_, H2}, {_, P2}} ->
			{ok, _} = usekeeper:add_user(RestUser, RestPass),
			case H2 of
				H2 when is_tuple(H2) ->
					{inet:ntoa(H2), P2};
				H2 when is_list(H2) ->
					{H2, P2}
			end;
		{false, {_, P2}} ->
			{ok, _} = usekeeper:add_user(RestUser, RestPass),
			{"localhost", P2}
	end,
	HostUrl = "https://" ++ Host ++ ":" ++ integer_to_list(Port),
	[{host_url, HostUrl}, {port, Port} | Config].

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	ok = usekeeper_test_lib:stop(),
	ok = application:stop(mnesia).

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
	[post_usage_specification, get_usage_specifications, delete_usage_specification,
	patch_usage_specification, post_usage, get_users, get_usage].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

get_users() ->
   [{userdata, [{doc, "Get the user collection."}]}].

get_users(Config) ->
   HostUrl = ?config(host_url, Config),
   CollectionUrl = HostUrl ++ "/party/v4/individual",
   Accept = {"accept", "application/json"},
   Request = {CollectionUrl, [Accept, auth_header()]},
   {ok, Result} = httpc:request(get, Request, [], []),
   {{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
   {_, "application/json"} = lists:keyfind("content-type", 1, Headers),
   ContentLength = integer_to_list(length(ResponseBody)),
   {_, ContentLength} = lists:keyfind("content-length", 1, Headers).


post_usage_specification() ->
	[{userdata, [{doc, "POST to Resource collection"}]}].

post_usage_specification(Config) ->
	HostUrl = ?config(host_url, Config),
	PathUsageSpec = ?PathUsage ++ "usageSpecification",
	CollectionUrl = HostUrl ++ PathUsageSpec,
	Name = random_string(10),
	BaseType = random_string(5),
	ClassType = random_string(10),
	Description = random_string(25),
	RequestBody = "{\n"
			++ "\t\"name\": \"" ++ Name ++ "\",\n"
			++ "\t\"description\": \"" ++ Description ++ "\",\n"
			++ "\t\"@baseType\": \"" ++ BaseType ++ "\",\n"
			++ "\t\"@type\": \"" ++ ClassType ++ "\",\n"
			++ "\t\"validFor\": {\n"
			++ "\t\t\"startDateTime\": \"2019-01-29T00:00\",\n"
			++ "\t\t\"endDateTime\": \"2019-12-31T23:59\"\n"
			++ "\t},\n"
			++ "\t\"usageSpecCharacteristic\": {\n"
			++ "\t\t\"name\": \"" ++ Name ++ "\",\n"
			++ "\t\t\"description\": \"" ++ Description ++ "\",\n"
			++ "\t\t\"configurable\": true,\n"
			++ "\t\t\"usageSpecCharacteristicValue\": [\n"
			++ "\t\t\t{\n"
			++ "\t\t\t\t\"valueType\": \"number\",\n"
			++ "\t\t\t\t\"default\": false\n"
			++ "\t\t\t}\n"
			++ "\t\t]\n"
			++ "\t}\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{ok, #{"id" := ID}} = zj:decode(ResponseBody),
	F = fun() ->
			mnesia:read(use_spec, ID, read)
	end,
	UsageSpec = case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, []} ->
			{error, not_found};
		{atomic, [Spec]} ->
			{ok, Spec}
	end,
	{ok, #use_spec{id = ID, name = Name, description = Description,
			class_type = ClassType, base_type = BaseType, characteristic = Char}} = UsageSpec,
	#{"name" := Name, "description" := Description,
			"configurable" := true, "usageSpecCharacteristicValue" := [CV]} = Char,
	#{"valueType" => "number", "default" => false} == CV.

get_usage_specifications() ->
	[{userdata, [{doc, "GET Usage Specification collection"}]}].

get_usage_specifications(Config) ->
	F = fun(_F, 0) ->
				ok;
			(F, N) ->
				UseSpec = usekeeper_test_lib:voice_spec(),
				{ok, #use_spec{}} = usekeeper:add_usage_spec(UseSpec),
				F(F, N - 1)
	end,
	ok = F(F, 5),
	HostUrl = ?config(host_url, Config),
	PathUsageSpec = ?PathUsage ++ "usageSpecification",
	CollectionUrl = HostUrl ++ PathUsageSpec,
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{ok, UsageSpecs} = zj:decode(ResponseBody),
	true = length(UsageSpecs) >= 5,
	true = lists:all(fun is_usage_spec/1, UsageSpecs).

delete_usage_specification() ->
	[{userdata, [{doc,"Delete usage specification for given Id"}]}].

delete_usage_specification(Config) ->
	UseSpec = usekeeper_test_lib:voice_spec(),
	{ok, #use_spec{id = Id}} = usekeeper:add_usage_spec(UseSpec),
	HostUrl = ?config(host_url, Config),
	URI = ?PathUsage ++ "usageSpecification/" ++ Id,
	Request = {HostUrl ++ URI, [auth_header()]},
	{ok, Result} = httpc:request(delete, Request, [], []),
	{{"HTTP/1.1", 204, _NoContent}, Headers, []} = Result,
	{_, "0"} = lists:keyfind("content-length", 1, Headers),
	F = fun() ->
			mnesia:read(use_spec, Id, read)
	end,
	{error, not_found} = case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, []} ->
			{error, not_found}
	end.

patch_usage_specification() ->
	[{userdata, [{doc,"Updates fields in usage specification for given Id"}]}].

patch_usage_specification(Config) ->
	Name = "DataSpec",
	CharValueSpec = #{"valueType" => "number", "from" => 1, "to" => 999},
	Characteristic = #{"name" => "originatingCountryCode",
			"configurable" => true,
			"usageSpecCharacteristicValue" => [CharValueSpec]},
	UsageSpec = #use_spec{name = "VoiceSpec", start_date = 1575027813168,
			end_date = 1675027813493, characteristic = Characteristic},
	{ok, #use_spec{id = Id}} = usekeeper:add_usage_spec(UsageSpec),
	Description = "Country code of the caller",
	RequestBody = "[\n"
			++ "\t{\n"
			++ "\t\t\"op\": \"add\",\n"
			++ "\t\t\"path\": \"/description\",\n"
			++ "\t\t\"value\": \"" ++ Description ++ "\"\n"
			++ "\t},\n"
			++ "\t{\n"
			++ "\t\t\"op\": \"add\",\n"
			++ "\t\t\"path\": \"/name\",\n"
			++ "\t\t\"value\": \"" ++ Name ++ "\"\n"
			++ "\t},\n"
			++ "\t{\n"
			++ "\t\t\"op\": \"replace\",\n"
			++ "\t\t\"path\": \"/description\",\n"
			++ "\t\t\"value\": \"UPDATED\"\n"
			++ "\t}\n"
			++ "]\n",
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathUsage ++ "usageSpecification/" ++ Id,
	Accept = "application/json",
	ContentType = "application/merge-patch+json",
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(patch, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/merge-patch+json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{ok, #{"id" := ID}} = zj:decode(ResponseBody),
	F = fun() ->
			mnesia:read(use_spec, ID, read)
	end,
	{ok, US}= case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, []} ->
			{error, not_found};
		{atomic, [Spec]} ->
			{ok, Spec}
	end,
	#use_spec{id = ID, name = Name, description = "UPDATED"} = US.

post_usage() ->
	[{userdata, [{doc, "POST to Usage collection"}]}].

post_usage(Config) ->
	HostUrl = ?config(host_url, Config),
	PathUsageSpec = ?PathUsage ++ "usage",
	CollectionUrl = HostUrl ++ PathUsageSpec,
	Description = random_string(25),
	RequestBody = "{\n"
			++ "\t\"type\": \"Voice\",\n"
			++ "\t\"date\": \"2013-04-19T16:42:23\",\n"
			++ "\t\"description\": \"" ++ Description ++ "\",\n"
			++ "\t\"status\": \"rated\",\n"
			++ "\t\"usageSpecification\": {\n"
			++ "\t\t\"id\": \"29\",\n"
			++ "\t\t\"name\": \"Voice usage specification\"\n"
			++ "\t\t},\n"
			++ "\t\"usageCharacteristic\": [\n"
			++ "\t\t{\n"
			++ "\t\t\"name\": \"originatingCountryCode\",\n"
			++ "\t\t\"value\": \"43\",\n"
			++ "\t\t},\n"
			++ "\t\t{\n"
			++ "\t\t\"name\": \"destinationCountryCode\",\n"
			++ "\t\t\"value\": \"49\"\n"
			++ "\t\t}\n"
			++ "\t],\n"
			++ "\t\"ratedProductUsage\": [\n"
			++ "\t\t{\n"
			++ "\t\t\"ratingDate\": \"2013-04-19T16:42:23\",\n"
			++ "\t\t\"taxIncludedRatingAmount\": \"12\",\n"
			++ "\t\t\"taxExcludedRatingAmount\": \"10\",\n"
			++ "\t\t\"productRef\": \"ref\",\n"
			++ "\t\t\"usageRatingTag\": \"Usage\",\n"
			++ "\t\t\"ratingAmountType\": \"Total\",\n"
			++ "\t\t\"taxRate\": \"20\",\n"
			++ "\t\t\"currencyCode\": \"EUR\",\n"
			++ "\t\t\"isBilled\": \"false\",\n"
			++ "\t\t\"isTaxExempt\": \"false\",\n"
			++ "\t\t\"offerTariffType\": \"Normal\"\n"
			++ "\t\t}\n"
			++ "\t]\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{ok, #{"usageCharacteristic" := UsageChar,
			"ratedProductUsage" := Rated}} = zj:decode(ResponseBody),
	true = is_list(UsageChar),
	true = is_list(Rated).

get_usage(Config) ->
	F = fun(_F, 0) ->
				ok;
			(F, N) ->
				Usage = usekeeper_test_lib:voice_usage(),
				{ok, _UsageLog} = usekeeper:add_usage(Usage),
				F(F, N - 1)
	end,
	ok = F(F, 5),
	HostUrl = ?config(host_url, Config),
	PathUsageSpec = ?PathUsage ++ "usage",
	CollectionUrl = HostUrl ++ PathUsageSpec,
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{ok, UsageLogs} = zj:decode(ResponseBody),
	true = length(UsageLogs) >= 5,
	true = lists:all(fun is_usage_log/1, UsageLogs).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

random_string(Length) ->
	Charset = lists:seq($a, $z),
	NumChars = length(Charset),
	Random = crypto:strong_rand_bytes(Length),
	random_string(Random, Charset, NumChars,[]).
random_string(<<N, Rest/binary>>, Charset, NumChars, Acc) ->
	CharNum = (N rem NumChars) + 1,
	NewAcc = [lists:nth(CharNum, Charset) | Acc],
	random_string(Rest, Charset, NumChars, NewAcc);
random_string(<<>>, _Charset, _NumChars, Acc) ->
	Acc.

basic_auth() ->
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	EncodeKey = base64:encode_to_string(RestUser ++ ":" ++ RestPass),
	"Basic " ++ EncodeKey.

auth_header() ->
	{"authorization", basic_auth()}.

is_usage_spec(#{"id" := Id, "name" := Name, "description" := Description,
		"validFor" := #{"startDateTime" := StartTime, "endDateTime" := EndTime},
		"usageSpecCharacteristic" := UsageSpecChars})
		when is_list(Id), is_list(Name), is_list(Description),
		is_list(StartTime), is_list(EndTime), is_map(UsageSpecChars) ->
	true;
is_usage_spec(_U) ->
	false.

is_usage_log([TS, N, #{"usageCharacteristic" := UsageChars,
		"ratedProductUsage" := Rated}]) when is_list(UsageChars),
		is_list(Rated), is_integer(TS), is_integer(N) ->
	true;
is_usage_log(_) ->
	false.
