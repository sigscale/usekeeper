%%% usekeeper_log_SUITE.erl
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
%%% Test suite for the log API of the
%%% {@link //usekeeper. usekeeper} application.
%%%
-module(usekeeper_log_SUITE).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% common_test test cases
-export([get_last/0, get_last/1]).

-include("usage.hrl").
-include_lib("common_test/include/ct.hrl").

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
	[get_last].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

get_last() ->
	[{userdata, [{doc, "Get last events from log"}]}].

get_last(_Config) ->
	FileSize = 1048576,
	NumFiles = 10,
	Term = {0, lists:duplicate(250, 0)},
	AverageSize = erlang:external_size({0, Term}),
	NumChunkItems = 65536 div AverageSize,
	{ok, Log} = disk_log:open([{name, make_ref()}, {file, "last"},
			{type, wrap}, {size, {FileSize, NumFiles}}]),
	Fill = fun(F, FileNum, ItemNum, 0) ->
				Info = disk_log:info(Log),
				case lists:keyfind(current_file, 1, Info) of
					{current_file, FileNum} ->
						ItemNum;
					{current_file, _} ->
						F(F, FileNum, ItemNum, NumChunkItems)
				end;
			(F, FileNum, ItemNum, N) ->
				NewItemNum = ItemNum + 1,
				R = rand:uniform(500),
				Item = {NewItemNum, lists:duplicate(R, 0)},
				disk_log:log(Log, Item),
				F(F, FileNum, NewItemNum, N - 1)
	end,
	% check with half full wrap log
	NumTotal1 = Fill(Fill, NumFiles div 2, 0, NumChunkItems),
	MaxSize = (NumChunkItems * 3) + 25,
	{MaxSize, Items1} = usekeeper_log:last(Log, MaxSize),
	Fcheck = fun({N, _}, N) ->
				N - 1
	end,
	StartItem1 = NumTotal1 - MaxSize,
	StartItem1 = lists:foldl(Fcheck, NumTotal1, Items1),
	% check while logging into last wrap file
	NumTotal2 = Fill(Fill, NumFiles, NumTotal1, NumChunkItems),
	{MaxSize, Items2} = usekeeper_log:last(Log, MaxSize),
	StartItem2 = NumTotal2 - MaxSize,
	StartItem2 = lists:foldl(Fcheck, NumTotal2, Items2),
	% check while logging into first file, after turnover
	NumTotal3 = Fill(Fill, 1, NumTotal2, NumChunkItems),
	{MaxSize, Items3} = usekeeper_log:last(Log, MaxSize),
	StartItem3 = NumTotal3 - MaxSize,
	StartItem3 = lists:foldl(Fcheck, NumTotal3, Items3),
	% check while logging into second file, after turnover
	NumTotal4 = Fill(Fill, 2, NumTotal3, NumChunkItems),
	{MaxSize, Items4} = usekeeper_log:last(Log, MaxSize),
	StartItem4 = NumTotal4 - MaxSize,
	StartItem4 = lists:foldl(Fcheck, NumTotal4, Items4).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------


