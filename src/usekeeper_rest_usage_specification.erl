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
%%% @doc This library module implements usage specification functions
%%% 	for a REST server in the {@link //usekeeper. usekeeper} application.
%%%
%%%   Handle `UsageSpecification' collection.
%%%
-module(usekeeper_rest_usage_specification).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([post_usage_specification/1]).
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
	["application/json"].

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
			{ok, #use_spec{href = Href, last_modified = LM} = UsageSpec} ->
				Headers = [{location, Href}, {etag, usekeeper_rest:etag(LM)}],
				Body = zj:encode(usage_specification(UsageSpec)),
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

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
usage_specification([href | T], #use_spec{href = Href} = R, Acc)
		when is_list(Href) ->
	usage_specification(T, R, Acc#{"href" => Href});
usage_specification([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	usage_specification(T, M, Acc#use_spec{href = Href});
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
		when is_list(UsageSpecChar), length(UsageSpecChar) > 0 ->
	usage_specification(T, R,
			Acc#{"usageSpecCharacteristic" => usage_spec_char(UsageSpecChar)});
usage_specification([characteristic | T],
		#{"usageSpecCharacteristic" := UsageSpecChar} = M, Acc)
		when is_list(UsageSpecChar) ->
	usage_specification(T, M,
			Acc#use_spec{characteristic = usage_spec_char(UsageSpecChar)});
usage_specification([_ | T], R, Acc) ->
	usage_specification(T, R, Acc);
usage_specification([], _, Acc) ->
	Acc.

-spec usage_spec_char(UsageSpecChar) -> UsageSpecChar
	when
		UsageSpecChar :: [specification_char()] | [map()].
%% @doc CODEC for `UsageSpecCharacteristic'.
usage_spec_char([#specification_char{} | _] = List) ->
	Fields = record_info(fields, specification_char),
	[usage_spec_char(Fields, R, #{}) || R <- List];
usage_spec_char([#{} | _] = List) ->
	Fields = record_info(fields, specification_char),
	[usage_spec_char(Fields, M, #specification_char{}) || M <- List];
usage_spec_char([]) ->
	[].
%% @hidden
usage_spec_char([name | T], #specification_char{name = Name} = R, Acc)
		when is_list(Name) ->
	usage_spec_char(T, R, Acc#{"name" => Name});
usage_spec_char([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	usage_spec_char(T, M, Acc#specification_char{name = Name});
usage_spec_char([description | T], #specification_char{description = Description} = R, Acc)
		when is_list(Description) ->
	usage_spec_char(T, R, Acc#{"description" => Description});
usage_spec_char([description | T], #{"description" := Description} = M, Acc)
		when is_list(Description) ->
	usage_spec_char(T, M, Acc#specification_char{description = Description});
usage_spec_char([configurable | T], #specification_char{configurable = Configurable} = R, Acc)
		when is_boolean(Configurable) ->
	usage_spec_char(T, R, Acc#{"configurable" => atom_to_list(Configurable)});
usage_spec_char([configurable | T], #{"configurable" := Configurable} = M, Acc)
		when is_list(Configurable) ->
	usage_spec_char(T, M, Acc#specification_char{configurable = list_to_existing_atom(Configurable)});
usage_spec_char([char_value | T], #specification_char{char_value = SpecCharValue} = R, Acc)
		when is_list(SpecCharValue), length(SpecCharValue) > 0 ->
	usage_spec_char(T, R, Acc#{"usageSpecCharacteristicValue" => spec_char_value(SpecCharValue)});
usage_spec_char([char_value | T], #{"usageSpecCharacteristicValue" := SpecCharValue} = M, Acc)
		when is_list(SpecCharValue) ->
	usage_spec_char(T, M, Acc#specification_char{char_value = spec_char_value(SpecCharValue)});
usage_spec_char([_ | T], R, Acc) ->
	usage_spec_char(T, R, Acc);
usage_spec_char([], _, Acc) ->
	Acc.

-spec spec_char_value(SpecCharValue) -> SpecCharValue
	when
		SpecCharValue :: [spec_char_value()] | [map()].
%% @doc CODEC for `UsageSpecCharacteristicValue'.
spec_char_value([#spec_char_value{} | _] = List) ->
	Fields = record_info(fields, spec_char_value),
	[spec_char_value(Fields, R, #{}) || R <- List];
spec_char_value([#{} | _] = List) ->
	Fields = record_info(fields, spec_char_value),
	[spec_char_value(Fields, M, #spec_char_value{}) || M <- List];
spec_char_value([]) ->
	[].
%% @hidden
spec_char_value([value_type | T], #spec_char_value{value_type = ValueType} = R, Acc)
		when is_list(ValueType) ->
	spec_char_value(T, R, Acc#{"valueType" => ValueType});
spec_char_value([value_type | T], #{"valueType" := ValueType} = M, Acc)
		when is_list(ValueType) ->
	spec_char_value(T, M, Acc#spec_char_value{value_type = ValueType});
spec_char_value([default | T], #spec_char_value{default = Default} = R, Acc)
		when is_boolean(Default) ->
	spec_char_value(T, R, Acc#{"default" => atom_to_list(Default)});
spec_char_value([default | T], #{"default" := Default} = M, Acc)
		when is_list(Default) ->
	spec_char_value(T, M, Acc#spec_char_value{default = list_to_existing_atom(Default)});
spec_char_value([value | T], #spec_char_value{value = Value} = R, Acc) ->
	spec_char_value(T, R, Acc#{"value" => Value});
spec_char_value([value | T], #{"value" := Value} = M, Acc) ->
	spec_char_value(T, M, Acc#spec_char_value{value = Value});
spec_char_value([from | T], #spec_char_value{from = From} = R, Acc) ->
	spec_char_value(T, R, Acc#{"valueFrom" => From});
spec_char_value([from | T], #{"valueFrom" := From} = M, Acc) ->
	spec_char_value(T, M, Acc#spec_char_value{from = From});
spec_char_value([to | T], #spec_char_value{to = To} = R, Acc) ->
	spec_char_value(T, R, Acc#{"valueTo" => To});
spec_char_value([value | T], #{"valueTo" := To} = M, Acc) ->
	spec_char_value(T, M, Acc#spec_char_value{to = To});
spec_char_value([_ | T], R, Acc) ->
	spec_char_value(T, R, Acc);
spec_char_value([], _, Acc) ->
	Acc.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

