%%% usekeeper_rest.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018-2019 SigScale Global Inc.
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
%%% @doc This library module implements utility functions
%%% 	for REST servers in the {@link //usekeeper. usekeeper} application.
%%%
-module(usekeeper_rest).
-copyright('Copyright (c) 2018-2019 SigScale Global Inc.').

-export([date/1, etag/1, iso8601/1]).
-export([parse_query/1, range/1, pointer/1, patch/2]).
-export([millionths_in/1, millionths_out/1]).

-include("usage.hrl").

%%----------------------------------------------------------------------
%%  The usekeeper_rest public API
%%----------------------------------------------------------------------

-spec date(DateTimeFormat) -> Result
	when
		DateTimeFormat :: pos_integer() | tuple(),
		Result :: calendar:datetime() | non_neg_integer().
%% @doc Convert iso8610 to date and time or
%%		date and time to timeStamp.
date(MilliSeconds) when is_integer(MilliSeconds) ->
	Seconds = 62167219200 + (MilliSeconds div 1000),
	calendar:gregorian_seconds_to_datetime(Seconds);
date(DateTime) when is_tuple(DateTime) ->
	Seconds = calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200,
	Seconds * 1000.

-spec etag(Etag) -> Etag
	when
		Etag :: string() | {TS, N},
		TS :: pos_integer(),
		N :: pos_integer().
%% @doc Map unique timestamp and HTTP ETag.
etag({TS, N} = _Etag) when is_integer(TS), is_integer(N)->
	integer_to_list(TS) ++ "-" ++ integer_to_list(N);
etag(Etag) when is_list(Etag) ->
	[TS, N] = string:tokens(Etag, "-"),
	{list_to_integer(TS), list_to_integer(N)}.

-spec range(Range) -> Result
	when
		Range :: RHS | {Start, End},
		RHS :: string(),
		Result :: {ok, {Start, End}} | {ok, RHS} | {error, 400},
		Start :: pos_integer(),
		End :: pos_integer().
%% @doc Parse or create a `Range' request header.
%% 	`RHS' should be the right hand side of an
%% 	RFC7233 `Range:' header conforming to TMF630
%% 	(e.g. "items=1-100").
%% @private
range(Range) when is_list(Range) ->
	try
		["items", S, E] = string:tokens(Range, "= -"),
		{ok, {list_to_integer(S), list_to_integer(E)}}
	catch
		_:_ ->
			{error, 400}
	end;
range({Start, End}) when is_integer(Start), is_integer(End) ->
	{ok, "items=" ++ integer_to_list(Start) ++ "-" ++ integer_to_list(End)}.

-spec iso8601(MilliSeconds) -> Result
	when
		MilliSeconds :: pos_integer() | string(),
		Result :: string() | pos_integer().
%% @doc Convert iso8610 to ISO 8601 format date and time.
iso8601(MilliSeconds) when is_integer(MilliSeconds) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = date(MilliSeconds),
	DateFormat = "~4.10.0b-~2.10.0b-~2.10.0b",
	TimeFormat = "T~2.10.0b:~2.10.0b:~2.10.0b.~3.10.0b",
	Chars = io_lib:fwrite(DateFormat ++ TimeFormat,
			[Year, Month, Day, Hour, Minute, Second, MilliSeconds rem 1000]),
	lists:flatten(Chars);
iso8601(ISODateTime) when is_list(ISODateTime) ->
	case string:rchr(ISODateTime, $T) of
		0 ->
			iso8601(ISODateTime, []);
		N ->
			iso8601(lists:sublist(ISODateTime, N - 1),
				lists:sublist(ISODateTime,  N + 1, length(ISODateTime)))
	end.
%% @hidden
iso8601(Date, Time) when is_list(Date), is_list(Time) ->
	D = iso8601_date(string:tokens(Date, ",-"), []),
	{H, Mi, S, Ms} = iso8601_time(string:tokens(Time, ":."), []),
	date({D, {H, Mi, S}}) + Ms.
%% @hidden
iso8601_date([[Y1, Y2, Y3, Y4] | T], _Acc) ->
	Y = list_to_integer([Y1, Y2, Y3, Y4]),
	iso8601_date(T, Y);
iso8601_date([[M1, M2] | T], Y) when is_integer(Y) ->
	M = list_to_integer([M1, M2]),
	iso8601_date(T, {Y, M});
iso8601_date([[D1, D2] | T], {Y, M}) ->
	D = list_to_integer([D1, D2]),
	iso8601_date(T, {Y, M, D});
iso8601_date([], {Y, M}) ->
	{Y, M, 1};
iso8601_date([], {Y, M, D}) ->
	{Y, M, D}.
%% @hidden
iso8601_time([H1 | T], []) ->
	H = list_to_integer(H1),
	iso8601_time(T, H);
iso8601_time([M1 | T], H) when is_integer(H) ->
	Mi = list_to_integer(M1),
	iso8601_time(T, {H, Mi});
iso8601_time([S1 | T], {H, Mi}) ->
	S = list_to_integer(S1),
	iso8601_time(T, {H, Mi, S});
iso8601_time([], {H, Mi}) ->
	{H, Mi, 0, 0};
iso8601_time([Ms1 | T], {H, Mi, S}) ->
	Ms = list_to_integer(Ms1),
	iso8601_time(T, {H, Mi, S, Ms});
iso8601_time([], {H, Mi, S}) ->
	{H, Mi, S, 0};
iso8601_time([], {H, Mi, S, Ms}) ->
	{H, Mi, S, Ms};
iso8601_time([], []) ->
	{0,0,0,0}.

-spec pointer(Path) -> Pointer
	when
		Path :: string(),
		Pointer :: [string()].
%% @doc Decode JSON Pointer.
%%    Apply the decoding rules of <a href="http://tools.ietf.org/html/rfc6901">RFC6901</a>.
%%    `Path' is a JSON string as used in the `"path"' member of a
%%    JSON Patch ((<a href="http://tools.ietf.org/html/rfc6902">RFC6902</a>)
%%    operation. `Pointer' is a list of member name strings in a path.
pointer(Pointer) ->
	pointer(Pointer, [], []).
%% @hidden
pointer([$/ | T], [], Acc) ->
	pointer(T, [], Acc);
pointer([$/ | T], Acc1, Acc2) ->
	pointer(T, [], [lists:reverse(Acc1) | Acc2]);
pointer([$- | T], Acc1, Acc2) ->
	pointer1(T, Acc1, Acc2);
pointer([H | T], Acc1, Acc2) ->
	pointer(T, [H | Acc1], Acc2);
pointer([], Acc1, Acc2) ->
	lists:reverse([lists:reverse(Acc1) | Acc2]).
%% @hidden
pointer1([$1 | T], Acc1, Acc2) ->
	pointer(T, [$/ | Acc1], Acc2);
pointer1([$0 | T], Acc1, Acc2) ->
	pointer(T, [$- | Acc1], Acc2);
pointer1(T, Acc1, Acc2) ->
	pointer(T, [$- | Acc1], Acc2).

-spec patch(Patch, Resource) -> Result
	when
		Patch :: [map()],
		Resource :: map(),
		Result :: map().
%% @doc Apply a JSON `Patch' (<a href="http://tools.ietf.org/html/rfc6902">RFC6902</a>).
%%    Modifies the `Resource' by applying the operations listed in `Patch'.
%%    `Operation' may be `"add"', `"remove"', or `"replace"'.
%%
patch([#{"op" := "add", "path" := Pointer,
		"value" := Value} | T] = _Patch, Resource) ->
	[Path] = pointer(Pointer),
	patch(T, Resource#{Path => Value});
patch([#{"op" := "replace", "path" := Pointer,
		"value" := Value} | T], Resource) ->
	[Path] = pointer(Pointer),
	case maps:is_key(Path, Resource) of
		true ->
			patch(T, Resource#{Path => Value});
		false ->
			mnesia:abort(400)
	end;
patch([#{"op" := "remove", "path" := Pointer} | T], Resource) ->
	[Path] = pointer(Pointer),
	patch(T, maps:remove(Path, Resource));
patch([], Resource) ->
	Resource.

-spec parse_query(Query) -> Result
	when
		Query :: string(),
		Result :: [{Key, Value}],
		Key :: string(),
		Value :: string().
%% @doc Parse the query portion of a URI.
%% @throws {error, 400}
parse_query("?" ++ Query) ->
	parse_query(Query);
parse_query(Query) when is_list(Query) ->
	parse_query(string:tokens(Query, "&"), []).
%% @hidden
parse_query([H | T], Acc) ->
	parse_query(T, parse_query1(H, string:chr(H, $=), Acc));
parse_query([], Acc) ->
	lists:reverse(Acc).
%% @hidden
parse_query1(_Field, 0, _Acc) ->
	throw({error, 400});
parse_query1(Field, N, Acc) ->
	Key = lists:sublist(Field, N - 1),
	Value = lists:sublist(Field, N + 1, length(Field)),
	[{Key, Value} | Acc].

-type millionths() :: non_neg_integer().
-spec millionths_in(In) -> Out
	when
		In :: string() | integer() | float(),
		Out :: millionths().
%% @doc Convert value from JSON to internal value.
%%
%% 	Internal representation is an integer value
%% 	representing the number of units, where a unit
%% 	is one millionth of a cent (1&#x000A2; = 1000000).
%%
millionths_in(In) when is_list(In) ->
	case string:tokens(In, [$.]) of
		[M] ->
			list_to_integer(M) * 1000000;
		[M, D] when length(D) =< 6 ->
			D1 = list_to_integer(D ++ lists:duplicate(6 - length(D), $0)),
			case list_to_integer(M) of
				M1 when M1 < 0 ->
					M1 * 1000000 - D1;
				M1 ->
					M1 * 1000000 + D1
			end
	end;
millionths_in(In) when is_integer(In) ->
	In * 1000000;
millionths_in(In) when is_float(In) ->
	millionths_in(float_to_list(In, [{decimals, 7}, compact])).

-spec millionths_out(In) -> Out
	when
		In :: millionths(),
		Out :: string().
%% @doc Convert internal value to string() for JSON.
%%
%% 	Internal representation is an integer value
%% 	representing the number of units, where a unit
%% 	is one millionth of a cent (1&#x000A2; = 1000000).
%%
millionths_out(In) when is_integer(In), In < 0 ->
	N1 = 0 - In,
	M = N1 div 1000000,
	D = N1 rem 1000000,
	SD = integer_to_list(D),
	S1 = [$-] ++ integer_to_list(M) ++ [$.]
			++ lists:duplicate(6 - length(SD), $0) ++ SD,
	S2 = string:strip(S1, right, $0),
	string:strip(S2, right, $.);
millionths_out(In) when is_integer(In) ->
	M = In div 1000000,
	D = In rem 1000000,
	SD = integer_to_list(D),
	S1 = integer_to_list(M) ++ [$.]
			++ lists:duplicate(6 - length(SD), $0) ++ SD,
	S2 = string:strip(S1, right, $0),
	string:strip(S2, right, $.).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

