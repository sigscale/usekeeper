%%% usekeeper_log.erl
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
%%% @doc This library module implements functions used in handling logging
%%%
%%%
-module(usekeeper_log).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

%% export the usekeeper_public API
-export([usage_open/0]).

%% exported the private function
-export([]).

-include("usage.hrl").

-define(USAGELOG, usage).

%%----------------------------------------------------------------------
%%  the usekeeper_log public api
%%----------------------------------------------------------------------

-spec usage_open() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc open an usage event disk log.
usage_open() ->
	{ok, Directory} = application:get_env(usekeeper, usage_log_dir),
	{ok, LogSize} = application:get_env(usekeeper, usage_log_size),
	{ok, LogFiles} = application:get_env(usekeeper, usage_log_files),
	{ok, LogNodes} = application:get_env(usekeeper, usage_log_nodes),
	open_log(Directory, ?USAGELOG, LogSize, LogFiles, LogNodes).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec open_log(Directory, Log, LogSize, LogFiles, LogNodes) -> Result
	when
		Directory  :: string(),
		Log :: atom(),
		LogSize :: integer(),
		LogFiles :: integer(),
		LogNodes :: [Node],
		Node :: atom(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc open disk log file
open_log(Directory, Log, LogSize, LogFiles, LogNodes) ->
	case file:make_dir(Directory) of
		ok ->
			open_log1(Directory, Log, LogSize, LogFiles, LogNodes);
		{error, eexist} ->
			open_log1(Directory, Log, LogSize, LogFiles, LogNodes);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
open_log1(Directory, Log, LogSize, LogFiles, LogNodes) ->
	FileName = Directory ++ "/" ++ atom_to_list(Log),
	case disk_log:open([{name, Log}, {file, FileName},
					{type, wrap}, {size, {LogSize, LogFiles}},
					{distributed, [node() | LogNodes]}]) of
		{ok, _} = Result ->
			open_log2(Log, [{node(), Result}], [], undefined);
		{repaired, _, _, _} = Result ->
			open_log2(Log, [{node(), Result}], [], undefined);
		{error, _} = Result ->
			open_log2(Log, [], [{node(), Result}], undefined);
		{OkNodes, ErrNodes} ->
			open_log2(Log, OkNodes, ErrNodes, undefined)
	end.
%% @hidden
open_log2(Log, OkNodes,
		[{Node, {error, {node_already_open, _}}} | T], Reason)
		when Node == node() ->
	open_log2(Log, [{Node, {ok, Log}} | OkNodes], T, Reason);
open_log2(Log, OkNodes, [{_, {error, {node_already_open, _}}} | T], Reason) ->
	open_log2(Log, OkNodes, T, Reason);
open_log2(Log, OkNodes, [{Node, Reason1} | T], Reason2) ->
	Descr = lists:flatten(disk_log:format_error(Reason1)),
	Trunc = lists:sublist(Descr, length(Descr) - 1),
	error_logger:error_report([Trunc, {module, ?MODULE},
		{log, Log}, {node, Node}, {error, Reason1}]),
	open_log2(Log, OkNodes, T, Reason2);
open_log2(_Log, OkNodes, [], Reason) ->
	case lists:keymember(node(), 1, OkNodes) of
		true ->
			ok;
		false ->
			{error, Reason}
	end.
