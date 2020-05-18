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
		Usage :: usage() | JSON,
		JSON :: map().
%% @doc CODEC for `Usage'.
usage(#{} = Usage) ->
	maps:fold(fun usage/3, #{}, Usage).
%% @hidden
usage(id, Id, Acc) when is_list(Id) ->
	Acc#{"id" => Id};
usage("id", Id, Acc) when is_list(Id) ->
	Acc#{id => Id};
usage(href, Href, Acc) when is_list(Href) ->
	Acc#{"href" => Href};
usage("href", Href, Acc) when is_list(Href) ->
	Acc#{href => Href};
usage(date, Date, Acc) when is_integer(Date) ->
	Acc#{"date" => usekeeper_rest:iso8601(Date)};
usage("date", Date, Acc) when is_list(Date) ->
	Acc#{date => usekeeper_rest:iso8601(Date)};
usage(usage_type, Type, Acc) when is_list(Type) ->
	Acc#{"usageType" => Type};
usage("usageType", Type, Acc) when is_list(Type) ->
	Acc#{usage_type => Type};
usage(type, Type, Acc) when is_list(Type) ->
	Acc#{"@type" => Type};
usage("@type", Type, Acc) when is_list(Type) ->
	Acc#{type => Type};
usage(base_type, BaseType, Acc) when is_list(BaseType) ->
	Acc#{"@baseType" => BaseType};
usage("@baseType", BaseType, Acc) when is_list(BaseType) ->
	Acc#{base_type => BaseType};
usage(description, Description, Acc) when is_list(Description) ->
	Acc#{"description" => Description};
usage("description", Description, Acc) when is_list(Description) ->
	Acc#{description => Description};
usage(status, received, Acc) ->
	Acc#{"status" => "received"};
usage("status", "received", Acc) ->
	Acc#{status => received};
usage(status, rejected, Acc) ->
	Acc#{"status" => "rejected"};
usage("status", "rejected", Acc) ->
	Acc#{status => rejected};
usage(status, recycled, Acc) ->
	Acc#{"status" => "recycled"};
usage("status", "recycled", Acc) ->
	Acc#{status => recycled};
usage(status, guided, Acc) ->
	Acc#{"status" => "guided"};
usage("status", "guided", Acc) ->
	Acc#{status => guided};
usage(status, rated, Acc) ->
	Acc#{"status" => "rated"};
usage("status", "rated", Acc) ->
	Acc#{status => rated};
usage(status, rerate, Acc) ->
	Acc#{"status" => "rerate"};
usage("status", "rerate", Acc) ->
	Acc#{status => rerate};
usage(status, billed, Acc) ->
	Acc#{"status" => "billed"};
usage("status", "billed", Acc) ->
	Acc#{status => billed};
usage(usage_specification, Specification, Acc) when is_map(Specification) ->
	Acc#{"usageSpecification" => maps:fold(fun ref_int/3, #{}, Specification)};
usage("usageSpecification", Specification, Acc) when is_map(Specification) ->
	Acc#{usage_specification => maps:fold(fun ref_ext/3, #{}, Specification)};
usage(usage_characteristic, UsageChar, Acc) when is_map(UsageChar) ->
	F = fun(Key, Value, Acc1) when is_list(Key) ->
			[#{"name" => Key, "value" => Value} | Acc1]
	end,
	Acc#{"usageCharacteristic" => maps:fold(F, [], UsageChar)};
usage("usageCharacteristic", UsageChar, Acc) when is_list(UsageChar) ->
	F = fun(#{"name" := Key, "value" := Value}, Acc1) when is_list(Key) ->
			Acc1#{Key => Value}
	end,
	Acc#{usage_characteristic => lists:foldl(F, #{}, UsageChar)};
usage(related_party, RelatedParty, Acc) when is_map(RelatedParty) ->
	F = fun(_, RelatedParty1, Acc1) ->
			[maps:fold(fun ref_int/3, #{}, RelatedParty1) | Acc1]
	end,
	Acc#{"relatedParty" => maps:fold(F, [], RelatedParty)};
usage("relatedParty", RelatedParty, Acc) when is_list(RelatedParty) ->
	F = fun(#{"id" := Id} = RelatedParty1, Acc1) ->
			Acc1#{Id => maps:fold(fun ref_ext/3, #{}, RelatedParty1)}
	end,
	Acc#{related_party => lists:foldl(F, #{}, RelatedParty)};
usage(rated_usage, RatedUsage, Acc) when is_map(RatedUsage) ->
	F = fun(_, RatedUsage1, Acc1) ->
			[maps:fold(fun rated_int/3, #{}, RatedUsage1) | Acc1]
	end,
	Acc#{"ratedProductUsage" => maps:fold(F, [], RatedUsage)};
usage("ratedProductUsage", RatedUsage, Acc) when is_list(RatedUsage) ->
	F = fun(#{"id" := Id} = RatedUsage1, Acc1) ->
				Acc1#{Id => maps:fold(fun rated_ext/3, #{}, RatedUsage1)};
			(#{} = RatedUsage1, Acc1) ->
				Id = integer_to_list(erlang:unique_integer([positive])),
				Acc1#{Id => maps:fold(fun rated_ext/3, #{},
						RatedUsage1#{"id" => Id})}
	end,
	Acc#{rated_usage => lists:foldl(F, #{}, RatedUsage)}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
ref_int(id, Id, Acc) when is_list(Id) ->
	Acc#{"id" => Id};
ref_int(href, Href, Acc) when is_list(Href) ->
	Acc#{"href" => Href};
ref_int(name, Name, Acc) when is_list(Name) ->
	Acc#{"name" => Name};
ref_int(type, Type, Acc) when is_list(Type) ->
	Acc#{"@type" => Type};
ref_int(base_type, BaseType, Acc) when is_list(BaseType) ->
	Acc#{"@baseType" => BaseType};
ref_int(schema_location, SchemaLocation, Acc) when is_list(SchemaLocation) ->
	Acc#{"@schemaLocation" => SchemaLocation};
ref_int(referred_type, ReferredType, Acc) when is_list(ReferredType) ->
	Acc#{"@referredType" => ReferredType};
ref_int(role, Role, Acc) when is_list(Role) ->
	Acc#{"role" => Role}.

%% @hidden
ref_ext("id", Id, Acc) when is_list(Id) ->
	Acc#{id => Id};
ref_ext("href", Href, Acc) when is_list(Href) ->
	Acc#{href => Href};
ref_ext("name", Name, Acc) when is_list(Name) ->
	Acc#{name => Name};
ref_ext("@type", Type, Acc) when is_list(Type) ->
	Acc#{type => Type};
ref_ext("@baseType", BaseType, Acc) when is_list(BaseType) ->
	Acc#{base_type => BaseType};
ref_ext("@schemaLocation", SchemaLocation, Acc) when is_list(SchemaLocation) ->
	Acc#{schema_location => SchemaLocation};
ref_ext("@referredType", ReferredType, Acc) when is_list(ReferredType) ->
	Acc#{referred_type => ReferredType};
ref_ext("role", Role, Acc) when is_list(Role) ->
	Acc#{role => Role}.

%% @hidden
rated_int(id, Id, Acc) when is_list(Id) ->
	Acc#{"id" => Id};
rated_int(href, Href, Acc) when is_list(Href) ->
	Acc#{"href" => Href};
rated_int(name, Name, Acc) when is_list(Name) ->
	Acc#{"name" => Name};
rated_int(type, Type, Acc) when is_list(Type) ->
	Acc#{"@type" => Type};
rated_int(base_type, BaseType, Acc) when is_list(BaseType) ->
	Acc#{"@baseType" => BaseType};
rated_int(schema_location, SchemaLocation, Acc) when is_list(SchemaLocation) ->
	Acc#{"@schemaLocation" => SchemaLocation};
rated_int(bucket_converted_in_amount, Amount, Acc) when is_integer(Amount) ->
	Acc#{"bucketValueConvertedInAmount" => usekeeper_rest:millionths_out(Amount)};
rated_int(currency_code, CurrencyCode, Acc) when is_list(CurrencyCode) ->
	Acc#{"currencyCode" => CurrencyCode};
rated_int(is_billed, IsBilled, Acc) when is_boolean(IsBilled) ->
	Acc#{"isBilled" => IsBilled};
rated_int(is_tax_exempt, IsTaxExempt, Acc) when is_boolean(IsTaxExempt) ->
	Acc#{"isTaxExempt" => IsTaxExempt};
rated_int(tariff_type, TariffType, Acc) when is_list(TariffType) ->
	Acc#{"offerTariffType" => TariffType};
rated_int(product_ref, ProductRef, Acc) when is_map(ProductRef) ->
	Acc#{"productRef" => maps:fold(fun ref_int/3, #{}, ProductRef)};
rated_int(product_ref, ProductRef, Acc) when is_list(ProductRef) ->
	Acc#{"productRef" => ProductRef};
rated_int(rating_amount_type, AmountType, Acc) when is_list(AmountType) ->
	Acc#{"ratingAmountType" => AmountType};
rated_int(rating_date, RatingDate, Acc) when is_integer(RatingDate) ->
	Acc#{"ratingDate" => usekeeper_rest:iso8601(RatingDate)};
rated_int(tax_excluded_amount, Amount, Acc) when is_integer(Amount) ->
	Acc#{"taxExcludedRatingAmount" => usekeeper_rest:millionths_out(Amount)};
rated_int(tax_included_amount, Amount, Acc) when is_integer(Amount) ->
	Acc#{"taxIncludedRatingAmount" => usekeeper_rest:millionths_out(Amount)};
rated_int(tax_rate, Rate, Acc) when is_integer(Rate) ->
	Acc#{"taxRate" => usekeeper_rest:millionths_out(Rate)};
rated_int(rating_tag, usage, Acc) ->
	Acc#{"usageRatingTag" => "usage"};
rated_int(rating_tag, included_usage, Acc) ->
	Acc#{"usageRatingTag" => "included usage"};
rated_int(rating_tag, non_included_usage, Acc) ->
	Acc#{"usageRatingTag" => "non included usage"}.

%% @hidden
rated_ext("id", Id, Acc) when is_list(Id) ->
	Acc#{id => Id};
rated_ext("name", Name, Acc) when is_list(Name) ->
	Acc#{name => Name};
rated_ext("@type", Type, Acc) when is_list(Type) ->
	Acc#{type => Type};
rated_ext("@baseType", BaseType, Acc) when is_list(BaseType) ->
	Acc#{base_type => BaseType};
rated_ext("@schemaLocation", SchemaLocation, Acc) when is_list(SchemaLocation) ->
	Acc#{schema_location => SchemaLocation};
rated_ext("bucketValueConvertedInAmount", Amount, Acc) when is_integer(Amount) ->
	Acc#{bucket_converted_in_amount => Amount};
rated_ext("bucketValueConvertedInAmount", Amount, Acc)
		when is_list(Amount); is_float(Amount) ->
	Acc#{bucket_converted_in_amount => usekeeper_rest:millionths_in(Amount)};
rated_ext("currencyCode", CurrencyCode, Acc) when is_list(CurrencyCode) ->
	Acc#{currency_code => CurrencyCode};
rated_ext("isBilled", IsBilled, Acc) when is_boolean(IsBilled) ->
	Acc#{is_billed => IsBilled};
rated_ext("isTaxExempt", IsTaxExempt, Acc) when is_boolean(IsTaxExempt) ->
	Acc#{is_tax_exempt => IsTaxExempt};
rated_ext("offerTariffType", TariffType, Acc) when is_list(TariffType) ->
	Acc#{tariff_type => TariffType};
rated_ext("productRef", ProductRef, Acc) when is_map(ProductRef) ->
	Acc#{product_ref => maps:fold(fun ref_ext/3, #{}, ProductRef)};
rated_ext("productRef", ProductRef, Acc) when is_list(ProductRef) ->
	Acc#{product_ref => ProductRef};
rated_ext("ratingAmountType", AmountType, Acc) when is_list(AmountType) ->
	Acc#{rating_amount_type => AmountType};
rated_ext("ratingDate", RatingDate, Acc) when is_list(RatingDate) ->
	Acc#{rating_date => usekeeper_rest:iso8601(RatingDate)};
rated_ext("taxExcludedRatingAmount", Amount, Acc) when is_integer(Amount) ->
	Acc#{tax_excluded_amount => Amount};
rated_ext("taxExcludedRatingAmount", Amount, Acc)
		when is_list(Amount); is_float(Amount) ->
	Acc#{tax_excluded_amount => usekeeper_rest:millionths_in(Amount)};
rated_ext("taxIncludedRatingAmount", Amount, Acc) when is_integer(Amount) ->
	Acc#{tax_included_amount => Amount};
rated_ext("taxIncludedRatingAmount", Amount, Acc)
		when is_list(Amount); is_float(Amount) ->
	Acc#{tax_included_amount => usekeeper_rest:millionths_in(Amount)};
rated_ext("taxRate", Rate, Acc) when is_integer(Rate) ->
	Acc#{tax_rate => Rate};
rated_ext("taxRate", Rate, Acc)
		when is_list(Rate); is_float(Rate) ->
	Acc#{tax_rate => usekeeper_rest:millionths_in(Rate)};
rated_ext("usageRatingTag", "usage", Acc) ->
	Acc#{rating_tag => usage};
rated_ext("usageRatingTag", "included usage", Acc) ->
	Acc#{rating_tag => included_usage};
rated_ext("usageRatingTag", "non included usage", Acc) ->
	Acc#{rating_tag => non_included_usage}.

%% @hidden
match([{Key, Value} | T], Acc) ->
	match(T, [{exact, Key, Value} | Acc]);
match([], Acc) ->
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
		QueryArgs = case length(Query) of
			N when N > 0 ->
				Rest = match(Query, []),
				Rest1 = [{array,[{complex, Rest}]}],
				parse_filter(Rest1);
			_ ->
				'_'
		end,
		MFA = [usekeeper, query_usage, [QueryArgs, CountOnly]],
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

-spec parse_filter(Query) -> Result
	when
		Query :: [term()],
		Result :: [{map(), [term()], [term()]}].
%% @doc Create `[MatchHead, MatchConditions]' from `Query'.
%% 	MatchHead = ets:match_pattern()
%%		MatchConditions = [tuple()]
%% @private
parse_filter(Query) ->
	parse_filter(Query, #{}, []).
%% @hidden
parse_filter([{array, [{complex, Filter}]}], MatchHead, MatchConditions) ->
	parse_filter(Filter, all, MatchHead, MatchConditions).
%% @hidden
parse_filter([{exact, "description", Description} | T], all, MatchHead, MatchConditions)
		when is_list(Description) ->
	parse_filter(T, all, MatchHead#{"description" => Description}, MatchConditions);
parse_filter([{exact, "date", Date} | T], all, MatchHead, MatchConditions)
		when is_list(Date) ->
	parse_filter(T, all, MatchHead#{"date" => Date}, MatchConditions);
parse_filter([{exact, "status", Status} | T], all, MatchHead, MatchConditions)
		when is_list(Status) ->
	parse_filter(T, all, MatchHead#{"status" => Status}, MatchConditions);
parse_filter([{exact, "type", Type} | T], all, MatchHead, MatchConditions)
		when is_list(Type) ->
	parse_filter(T, all, MatchHead#{"type" => Type}, MatchConditions);
parse_filter([{exact, "usageSpecification" ++ "." ++ "id", Id} | T],
		all, MatchHead, MatchConditions) when is_list(Id) ->
	parse_filter(T, all, MatchHead#{"usageSpecification" => #{"id" => Id}}, MatchConditions);
parse_filter([{exact, "ratedProductUsage" ++ "." ++ "taxIncludedRatingAmount", TaxIncluded} | T],
		all, MatchHead, MatchConditions) when is_list(TaxIncluded) ->
	parse_filter(T, all, MatchHead#{"ratedProductUsage" =>
			[#{"taxIncludedRatingAmount" => list_to_integer(TaxIncluded)}]}, MatchConditions);
parse_filter([{exact, "ratedProductUsage" ++ "." ++ "taxExcludedRatingAmount", TaxExcluded} | T],
		all, MatchHead, MatchConditions) when is_list(TaxExcluded) ->
	parse_filter(T, all, MatchHead#{"ratedProductUsage" =>
			[#{"taxExcludedRatingAmount" => list_to_integer(TaxExcluded)}]}, MatchConditions);
parse_filter([{exact, "ratedProductUsage" ++ "." ++ "taxRate", TaxRate} | T],
		all, MatchHead, MatchConditions) when is_list(TaxRate) ->
	parse_filter(T, all, MatchHead#{"ratedProductUsage" =>
			[#{"taxRate" => list_to_integer(TaxRate)}]}, MatchConditions);
parse_filter([], all, MatchHead, MatchConditions) ->
	[{MatchHead, MatchConditions, ['$_']}].

