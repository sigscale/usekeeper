%%% usekeeper_rest_res_usage_specification.erl
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
%%%   Handle `UsageSpecification' collection.
%%%
-module(usekeeper_rest_res_usage_specification).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([post_usage_specification/1, get_usage_specifications/3,
		delete_usage_specification/1, patch_usage_specification/4]).
-export([usage_specification/1]).

-include("usage.hrl").

%%----------------------------------------------------------------------
%%  The usekeeper public API
%%----------------------------------------------------------------------

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Returns list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/merge-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Returns list of resource representations available.
content_types_provided() ->
	["application/json"].

-spec post_usage_specification(RequestBody) -> Result
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Handle `POST' request on `UsageSpecification' collection.
post_usage_specification(RequestBody) ->
	try
		{ok, UsageSpecMap} = zj:decode(RequestBody),
		case usekeeper:add_usage_spec(usage_specification(UsageSpecMap)) of
			{ok, #use_spec{last_modified = LM} = UsageSpec} ->
				Headers = [{etag, usekeeper_rest:etag(LM)}],
				Body = zj:encode(usage_specification(UsageSpec)),
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

-spec get_usage_specifications(Method, Query, Headers) -> Result
	when
		Method :: string(), % "GET"
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET /usageManagement/v4/usageSpecification'
%% 	requests.
get_usage_specifications(Method, Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_usage_specifications(Method, NewQuery, Filters, Headers);
		false ->
			get_usage_specifications(Method, Query, [], Headers)
	end.
%% @hidden
get_usage_specifications(Method, Query, Filters, Headers) ->
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

-spec delete_usage_specification(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Handle `DELETE' request on a `Usage Specification' resource.
delete_usage_specification(Id) ->
	case usekeeper:delete_usage_spec(Id) of
		ok ->
			{ok, [], []};
		{error, _Reason} ->
			{error, 400}
	end.

-spec patch_usage_specification(Id, Etag, ContentType, ReqBody) -> Result
	when
		Id :: string(),
		Etag :: undefined | string(),
		ContentType :: string(),
		ReqBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()} .
%% @doc Update an existing `Usage Specification'.
%%
%% 	Respond to `PATCH /usageManagement/v4/usageSpecification/{Id}' request.
%%
patch_usage_specification(Id, Etag, "application/merge-patch+json", ReqBody) ->
	try
		case Etag of
			undefined ->
				{undefined, zj:decode(ReqBody)};
			Etag ->
				{usekeeper_rest:etag(Etag) , zj:decode(ReqBody)}
		end
	of
		{EtagT, {ok, Patch}} ->
			F = fun() ->
					case mnesia:read(use_spec, Id, write) of
						[#use_spec{last_modified = LM}]
								when EtagT /= undefined, LM /= EtagT ->
							mnesia:abort(412);
						[#use_spec{} = UsageSpec] ->
							case catch usage_specification(usekeeper_rest:patch(Patch,
									usage_specification(UsageSpec))) of
								#use_spec{} = US ->
									TS = erlang:system_time(millisecond),
									N = erlang:unique_integer([positive]),
									LM = {TS, N},
									NewUsageSpec = US#use_spec{last_modified = LM},
									mnesia:write(use_spec, NewUsageSpec, write),
									NewUsageSpec;
								_ ->
									mnesia:abort(400)
							end;
						[] ->
							mnesia:abort(404)
					end
			end,
			case mnesia:transaction(F) of
				{atomic, #use_spec{last_modified = LM} = NewUsageSpec} ->
					Body = zj:encode(usage_specification(NewUsageSpec)),
					Headers = [{content_type, "application/merge-patch+json"},
							{etag, usekeeper_rest:etag(LM)}],
					{ok, Headers, Body};
				{aborted, Status} when is_integer(Status) ->
					{error, Status};
				{aborted, _Reason} ->
					{error, 500}
			end;
		_ ->
			{error, 400}
	catch
		_:_ ->
			{error, 400}
	end;
patch_usage_specification(_, _, "application/json", _) ->
	{error, 415}.

-spec usage_specification(UsageSpec) -> UsageSpec
	when
		UsageSpec :: use_spec() | map().
%% @doc CODEC for `UsageSpecification'.
usage_specification(#use_spec{} = UsageSpec) ->
	usage_specification(record_info(fields, use_spec), UsageSpec, #{});
usage_specification(#{} = UsageSpecMap) ->
	usage_specification(record_info(fields, use_spec), UsageSpecMap, #use_spec{}).
%% @hidden
usage_specification([id | T], #use_spec{id = Id} = R, Acc)
		when is_list(Id) ->
	usage_specification(T, R, Acc#{"id" => Id});
usage_specification([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	usage_specification(T, M, Acc#use_spec{id = Id});
usage_specification([name | T], #use_spec{name = Name} = R, Acc)
		when is_list(Name) ->
	usage_specification(T, R, Acc#{"name" => Name});
usage_specification([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	usage_specification(T, M, Acc#use_spec{name = Name});
usage_specification([description | T],
		#use_spec{description = Description} = R, Acc)
		when is_list(Description) ->
	usage_specification(T, R, Acc#{"description" => Description});
usage_specification([description | T], #{"description" := Description} = M,
		Acc) when is_list(Description) ->
	usage_specification(T, M, Acc#use_spec{description = Description});
usage_specification([class_type | T], #use_spec{class_type = Type} = R, Acc)
		when is_list(Type) ->
	usage_specification(T, R, Acc#{"@type" => Type});
usage_specification([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	usage_specification(T, M, Acc#use_spec{class_type = Type});
usage_specification([base_type | T], #use_spec{base_type = Type} = R, Acc)
		when is_list(Type) ->
	usage_specification(T, R, Acc#{"@baseType" => Type});
usage_specification([base_type | T], #{"@baseType" := Type} = M, Acc)
		when is_list(Type) ->
	usage_specification(T, M, Acc#use_spec{base_type = Type});
usage_specification([start_date | T], #use_spec{start_date = StartDate} = R,
		Acc) when is_integer(StartDate) ->
	ValidFor = #{"startDateTime" => usekeeper_rest:iso8601(StartDate)},
	usage_specification(T, R, Acc#{"validFor" => ValidFor});
usage_specification([start_date | T],
		#{"validFor" := #{"startDateTime" := Start}} = M, Acc)
		when is_list(Start) ->
	usage_specification(T, M, Acc#use_spec{start_date = usekeeper_rest:iso8601(Start)});
usage_specification([end_date | T], #use_spec{end_date = End} = R,
		#{"validFor" := ValidFor} = Acc) when is_integer(End) ->
	NewValidFor = ValidFor#{"endDateTime" => usekeeper_rest:iso8601(End)},
	usage_specification(T, R, Acc#{"validFor" := NewValidFor});
usage_specification([end_date | T], #use_spec{end_date = End} = R, Acc)
		when is_integer(End) ->
	ValidFor = #{"endDateTime" => usekeeper_rest:iso8601(End)},
	usage_specification(T, R, Acc#{"validFor" := ValidFor});
usage_specification([end_date | T],
		#{"validFor" := #{"endDateTime" := End}} = M, Acc)
		when is_list(End) ->
	usage_specification(T, M, Acc#use_spec{end_date = usekeeper_rest:iso8601(End)});
usage_specification([characteristic | T],
		#use_spec{characteristic = UsageSpecChar} = R, Acc)
		when is_map(UsageSpecChar) ->
	F = fun(_Key, Value, AccIn) ->
				[Value | AccIn]
	end,
	usage_specification(T, R,
			Acc#{"usageSpecCharacteristic" => maps:fold(F, [], UsageSpecChar)});
usage_specification([characteristic | T],
		#{"usageSpecCharacteristic" := UsageSpecCharArray} = M, Acc)
		when is_list(UsageSpecCharArray) ->
	CharList = [{Name, CharMap} || #{"name" := Name} = CharMap <- UsageSpecCharArray],
	usage_specification(T, M,
			Acc#use_spec{characteristic = maps:from_list(CharList)});
usage_specification([_H | T], R, Acc) ->
	usage_specification(T, R, Acc);
usage_specification([], _, Acc) ->
	Acc.

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
		MFA = [usekeeper, query, [use_spec, '_'] ++ [CountOnly]],
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
			UsageSpecs = lists:map(fun usage_specification/1, Events),
			Body = zj:encode(UsageSpecs),
			Headers = [{content_type, "application/json"},
				{etag, Etag}, {accept_ranges, "items"},
				{content_range, ContentRange}],
			{ok, Headers, Body}
	end.
