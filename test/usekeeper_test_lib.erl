%%% usekeeper_test_lib.erl
%%% vim: ts=3
%%%
-module(usekeeper_test_lib).

-export([initialize_db/0, start/0, stop/0, voice_spec/0, voice_usage/0]).

-include("usage.hrl").

initialize_db() ->
	case mnesia:system_info(is_running) of
		no ->
			ok = application:start(mnesia),
			initialize_db();
		S when S == starting; S == stopping ->
			receive
				after 1000 ->
					initialize_db()
			end;
		yes ->
			Tables = [httpd_group, httpd_user, use_spec],
			case mnesia:wait_for_tables(Tables, 1000) of
				{timeout, _} ->
					ok = application:stop(mnesia),
					{ok, _} = usekeeper_app:install(),
					initialize_db();
				ok ->
					ok
			end
	end.

start() ->
	start([crypto, inets, ssl, usekeeper]).

start([H | T]) ->
	case application:start(H) of
		ok  ->
			start(T);
		{error, {already_started, H}} ->
			start(T);
		{error, Reason} ->
			{error, Reason}
	end;
start([]) ->
	ok.

stop() ->
	application:stop(usekeeper).

voice_spec() ->
	Name = "VoiceSpec",
	Description = "Specification for voice call usage",
	Time = erlang:system_time(millisecond),
	StartDate = usekeeper_rest:iso8601(Time),
	EndDate = usekeeper_rest:iso8601(Time + (1440 * 60 * 30 * 1000)),
	CharName1 = "originatingCountryCode",
	CharDescription1 = "Country code of the caller",
	CharConfig1 = true,
	CharValueType1 = "number",
	CharValueFrom1 = 1,
	CharValueTo1 = 999,
	CharValueSpec1 = #{"valueType" => CharValueType1,
			"from" => CharValueFrom1, "to" => CharValueTo1},
	CharSpec1 = #{"name" => CharName1,
			"description" => CharDescription1,
			"configurable" => CharConfig1,
			"usageSpecCharacteristicValue" => [CharValueSpec1]},
	CharName2 = "originatingNumber",
	CharDescription2 = "Caller's directory number",
	CharConfig2 = true,
	CharValueType2 = "string",
	CharValueSpec2 = #{"valueType" => CharValueType2},
	CharSpec2 = #{"name" => CharName2,
			"description" => CharDescription2,
			"configurable" => CharConfig2,
			"usageSpecCharacteristicValue" => [CharValueSpec2]},
	CharName3 = "destinationCountryCode",
	CharDescription3 = "Country code of the callee",
	CharConfig3 = true,
	CharValueType3 = "number",
	CharValueFrom3 = 1,
	CharValueTo3 = 999,
	CharValueSpec3 = #{"valueType" => CharValueType3,
			"from" => CharValueFrom3, "to" => CharValueTo3},
	CharSpec3 = #{"name" => CharName3,
			"description" => CharDescription3,
			"configurable" => CharConfig3,
			"usageSpecCharacteristicValue" => [CharValueSpec3]},
	CharName4 = "destinationNumber",
	CharDescription4 = "Callee's directory number",
	CharConfig4 = true,
	CharValueType4 = "string",
	CharValueSpec4 = #{"valueType" => CharValueType4},
	CharSpec4 = #{"name" => CharName4,
			"description" => CharDescription4,
			"configurable" => CharConfig4,
			"usageSpecCharacteristicValue" => [CharValueSpec4]},
	CharName5 = "duration",
	CharDescription5 = "Duration of the call",
	CharConfig5 = true,
	CharValueType5 = "number",
	CharValueFrom5 = 0,
	CharValueSpec5 = #{"valueType" => CharValueType5,
			"from" => CharValueFrom5},
	CharSpec5 = #{"name" => CharName5,
			"description" => CharDescription5,
			"configurable" => CharConfig5,
			"usageSpecCharacteristicValue" => [CharValueSpec5]},
	CharName6 = "startDateTime",
	CharDescription6 = "Start time of the call",
	CharConfig6 = true,
	CharValueType6 = "dateTime",
	CharValueSpec6 = #{"valueType" => CharValueType6},
	CharSpec6 = #{"name" => CharName6,
			"description" => CharDescription6,
			"configurable" => CharConfig6,
			"usageSpecCharacteristicValue" => [CharValueSpec6]},
	CharName7 = "answerDateTime",
	CharDescription7 = "Time the call was answered",
	CharConfig7 = true,
	CharValueType7 = "dateTime",
	CharValueSpec7 = #{"valueType" => CharValueType7},
	CharSpec7 = #{"name" => CharName7,
			"description" => CharDescription7,
			"configurable" => CharConfig7,
			"usageSpecCharacteristicValue" => [CharValueSpec7]},
	CharName8 = "endDateTime",
	CharDescription8 = "End time of the call",
	CharConfig8 = true,
	CharValueType8 = "dateTime",
	CharValueSpec8 = #{"valueType" => CharValueType8},
	CharSpec8 = #{"name" => CharName8,
			"description" => CharDescription8,
			"configurable" => CharConfig8,
			"usageSpecCharacteristicValue" => [CharValueSpec8]},
	#use_spec{name = Name, description = Description,
			start_date = StartDate, end_date = EndDate,
			characteristic = #{"usageSpecChar1" => CharSpec1,
					"usageSpecChar2" => CharSpec2, "usageSpecChar3" => CharSpec3,
					"usageSpecChar4" => CharSpec4, "usageSpecChar5" => CharSpec5,
					"usageSpecChar6" => CharSpec6, "usageSpecChar7" => CharSpec7,
					"usageSpecChar8" => CharSpec8}}.

voice_usage() ->
	Description = "voice",
	RelatedId1 = "15",
	RelatedId2 = "23",
	Date = usekeeper_rest:iso8601(erlang:system_time(millisecond)),
	CharName1 = "region",
	CharName2 = "metro",
	CharName3 = "codec",
	CharName4 = "priority",
	UsageChar1 = #{"name" => CharName1, "value" => 1},
	UsageChar2 = #{"name" => CharName2, "value" => "Gotham"},
	UsageChar3 = #{"name" => CharName3, "value" => "amr"},
	UsageChar4 = #{"name" => CharName4, "value" => "low"},
	RelatedParty1 = #{"id" => RelatedId1, "role" => "customer"},
	RelatedParty2 = #{"id" => RelatedId2, "role" => "supplier"},
	TaxExcluded = 10000000,
	TaxRate = 400000,
	TaxIncluded = TaxExcluded + ((TaxExcluded * TaxRate) div 100),
	Rated = #{"ratingDate" => Date, "taxIncludedRatingAmount" => TaxIncluded,
			"taxExcludedRatingAmount" => TaxExcluded,
			"productRef" => #{"id" => "12345",
					"href" => "/productInventoryManagement/v4/product/12345"},
			"usageRatingTag" => "usage", "ratingAmountType" => "total",
			"taxRate" => TaxRate, "isTaxExempt" => false, "currencyCode" => "EUR",
			"isBilled" => false, "offerTariffType" => "normal"},
	#{"description" => Description,
			"date" => Date, "status" => "received",
			"usageType" => "voice",
			"usageSpecification" => #{"id" => "234",
					"href" => "/usageManagement/v4/usageSpecification/234",
					"name" => "Voice"},
			"usageCharacteristic" => [UsageChar1, UsageChar2,
					UsageChar3, UsageChar4],
			"relatedParty" => [RelatedParty1,RelatedParty2],
			"ratedProductUsage" => [Rated]}.

