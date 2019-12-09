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
-export([post_usage/1]).

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
			{ok, {TS, N, U}} ->
				UsageLog = {TS, N, usage(U)},
				Body = zj:encode(UsageLog),
				Headers = [{content_type, "application/json"}],
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
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
	UsageCharMap = list_to_map(UsageChar, 1, #{}),
	usage(T, U#{"usageCharacteristic" => UsageCharMap});
usage(["ratedProductUsage" | T],
		#{"ratedProductUsage" := Rated} = U) when is_map(Rated) ->
	I = maps:iterator(Rated),
	RatedList = map_to_list(maps:next(I), []),
	usage(T, U#{"ratedProductUsage" => RatedList});
usage(["ratedProductUsage" | T],
		#{"ratedProductUsage" := Rated} = U) when is_list(Rated) ->
	RatedMap = list_to_map(Rated, 1, #{}),
	usage(T, U#{"ratedProductUsage" => RatedMap});
usage(["relatedParty" | T],
		#{"relatedParty" := Related} = U) when is_map(Related) ->
	I = maps:iterator(Related),
	RelatedList = map_to_list(maps:next(I), []),
	usage(T, U#{"relatedParty" => RelatedList});
usage(["relatedParty" | T],
		#{"relatedParty" := Related} = U) when is_list(Related) ->
	RelatedMap = list_to_map(Related, 1, #{}),
	usage(T, U#{"relatedParty" => RelatedMap});
usage([_H | T], Usage) ->
	usage(T, Usage);
usage([], Usage) ->
	Usage.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

list_to_map([H | T], N, Acc) ->
	Key = integer_to_list(N),
	list_to_map(T, N + 1, Acc#{Key => H});
list_to_map([], _N, Acc) ->
	Acc.

map_to_list({_Key, Value, I}, Acc) ->
	map_to_list(maps:next(I), [Value | Acc]);
map_to_list(none, Acc) ->
	Acc.
