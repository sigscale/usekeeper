%%% usekeeper_rest_res_usage.erl
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
%%% @doc This library module implements usage specification functions
%%% 	for a REST server in the {@link //usekeeper. usekeeper} application.
%%%
%%%   Handle `Usage' collection.
%%%
-module(usekeeper_rest_res_usage).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([post_usage/1, get_usage/3]).

-export([usage/1]).

-include("usage.hrl").

%%----------------------------------------------------------------------
%%  The usekeeper public API
%%----------------------------------------------------------------------

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Returns list of resource representations accepted.
content_types_accepted() ->
	["application/json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Returns list of resource representations available.
content_types_provided() ->
	["application/json"].

-spec post_usage(RequestBody) -> Result
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Handle `POST' request on `Usage' collection.
post_usage(RequestBody) ->
	try
		{ok, Usage} = zj:decode(RequestBody),
		case usekeeper:add_usage(usage(Usage)) of
			{ok, {_TS, _N, U}} ->
				Body = zj:encode(usage(U)),
				Headers = [{content_type, "application/json"}],
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

-spec get_usage(Method, Query, Headers) -> Result
	when
		Method :: string(), % "GET"
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET /usageManagement/v4/usage'
%% 	requests.
get_usage(Method, Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_usage(Method, NewQuery, Filters, Headers);
		false ->
			get_usage(Method, Query, [], Headers)
	end.
%% @hidden
get_usage(Method, Query, Filters, Headers) ->
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
		{false, false, {"range", "items=1-" ++ _ = Range}} ->
			case usekeeper_rest:range(Range) of
				{error, _} ->
					{error, 400};
				{ok, {Start, End}} ->
					query_start(Method, Query, Filters, Start, End)
			end;
		{false, false, {"range", _Range}} ->
			{error, 416};
		{false, false, false} ->
			query_start(Method, Query, Filters, undefined, undefined)
	end.

-spec usage(Usage) -> Usage
	when
		Usage :: map().
%% @doc CODEC for `Usage'.
usage(#{} = Usage) ->
	usage(maps:keys(Usage), Usage).
%% @hidden
usage(["usageCharacteristic" | T],
		#{"usageCharacteristic" := UsageChar} = U) when is_map(UsageChar) ->
	I = maps:iterator(UsageChar),
	UsageCharList = map_to_list(maps:next(I), []),
	usage(T, U#{"usageCharacteristic" => UsageCharList});
usage(["usageCharacteristic" | T],
		#{"usageCharacteristic" := UsageChar} = U) when is_list(UsageChar) ->
	CharList = [{Name, CharMap} || #{"name" := Name} = CharMap <- UsageChar],
	usage(T, U#{"usageCharacteristic" => maps:from_list(CharList)});
usage(["relatedParty" | T],
		#{"relatedParty" := Related} = U) when is_map(Related) ->
	I = maps:iterator(Related),
	RelatedList = map_to_list(maps:next(I), []),
	usage(T, U#{"relatedParty" => RelatedList});
usage(["relatedParty" | T],
		#{"relatedParty" := Related} = U) when is_list(Related) ->
	RelatedList = [{Id, RelatedMap} || #{"id" := Id} = RelatedMap <- Related],
	usage(T, U#{"relatedParty" => maps:from_list(RelatedList)});
usage([_H | T], Usage) ->
	usage(T, Usage);
usage([], Usage) ->
	Usage.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
map_to_list({_Key, Value, I}, Acc) ->
	map_to_list(maps:next(I), [Value | Acc]);
map_to_list(none, Acc) ->
	Acc.

%% @hidden
query_start(Method, Query, Filters, RangeStart, RangeEnd) ->
	try
		CountOnly = case Method of
			"GET" ->
				false;
			"HEAD" ->
				true
		end,
		MFA = [usekeeper, query_usage, ['_'] ++ [CountOnly]],
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
			Usages = [usage(Usage) || {_TS, _N, Usage} <- Events],
			Body = zj:encode(Usages),
			Headers = [{content_type, "application/json"},
				{etag, Etag}, {accept_ranges, "items"},
				{content_range, ContentRange}],
			{ok, Headers, Body}
	end.
