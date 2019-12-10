%%% usekeeper_app.erl
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
%%% @doc This {@link //stdlib/application. application} behaviour callback
%%% 	module starts and stops the {@link //usekeeper. usekeeper} application.
%%%
-module(usekeeper_app).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

-behaviour(application).

%% callbacks needed for application behaviour
-export([start/2, stop/1, config_change/3]).

%% optional callbacks for application behaviour
-export([prep_stop/1, start_phase/3]).

%% export the usekeeper_app private API for installation
-export([install/0, install/1]).

-include("usage.hrl").
-include_lib("inets/include/mod_auth.hrl").

-record(state, {}).

-define(WAITFORSCHEMA, 10000).
-define(WAITFORTABLES, 10000).

%%----------------------------------------------------------------------
%%  The usekeeper_app aplication callbacks
%%----------------------------------------------------------------------

-type start_type() :: normal | {takeover, node()} | {failover, node()}.
-spec start(StartType, StartArgs) -> Result
	when
		StartType :: start_type(),
		StartArgs :: term(),
		Result :: {'ok', pid()} | {'ok', pid(), State} | {'error', Reason},
		State :: #state{},
		Reason :: term().
%% @doc Starts the application processes.
%% @see //kernel/application:start/1
%% @see //kernel/application:start/2
%%
start(normal = _StartType, _Args) ->
	Tables = [use_spec],
	case mnesia:wait_for_tables(Tables, 60000) of
		ok ->
			start2();
		{timeout, BadTabList} ->
			case force(BadTabList) of
				ok ->
					start2();
				{error, Reason} ->
					error_logger:error_report(["usekeeper application failed to start",
							{reason, Reason}, {module, ?MODULE}]),
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start2() ->
	{ok, ExportDir} = application:get_env(export_dir),
	case create_dir(ExportDir) of
		ok ->
			start3();
		{error, Reason} ->
			error_logger:error_report(["usekeeper application failed to start",
					{reason, Reason}, {module, ?MODULE}]),
			{error, Reason}
	end.
%% @hidden
start3() ->
	case inets:services_info() of
		ServicesInfo when is_list(ServicesInfo) ->
			{ok, Profile} = application:get_env(hub_profile),
			start4(Profile, ServicesInfo);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start4(Profile, [{httpc, _Pid, Info} | T]) ->
	case proplists:lookup(profile, Info) of
		{profile, Profile} ->
			start5(Profile);
		_ ->
			start4(Profile, T)
	end;
start4(Profile, [_ | T]) ->
	start4(Profile, T);
start4(Profile, []) ->
	case inets:start(httpc, [{profile, Profile}]) of
		{ok, _Pid} ->
			start5(Profile);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start5(Profile) ->
	{ok, Options} = application:get_env(hub_options),
	case httpc:set_options(Options, Profile) of
		ok ->
			supervisor:start_link(usekeeper_sup, []);
		{error, Reason} ->
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%%  The usekeeper_app private API
%%----------------------------------------------------------------------

-spec install() -> Result
	when
		Result :: {ok, Tables},
		Tables :: [atom()].
%% @equiv install([node() | nodes()])
install() ->
	Nodes = [node() | nodes()],
	install(Nodes).

-spec install(Nodes) -> Result
	when
		Nodes :: [node()],
		Result :: {ok, Tables},
		Tables :: [atom()].
%% @doc Initialize SigScale UseKeeper tables.
%% 	`Nodes' is a list of the nodes where
%% 	{@link //usekeeper. usekeeper} tables will be replicated.
%%
%% 	If {@link //mnesia. mnesia} is not running an attempt
%% 	will be made to create a schema on all available nodes.
%% 	If a schema already exists on any node
%% 	{@link //mnesia. mnesia} will be started on all nodes
%% 	using the existing schema.
%%
%% @private
%%
install(Nodes) when is_list(Nodes) ->
	case mnesia:system_info(is_running) of
		no ->
			case mnesia:create_schema(Nodes) of
				ok ->
					error_logger:info_report("Created mnesia schema",
							[{nodes, Nodes}]),
					install1(Nodes);
				{error, Reason} ->
					error_logger:error_report(["Failed to create schema",
							mnesia:error_description(Reason),
							{nodes, Nodes}, {error, Reason}]),
					{error, Reason}
			end;
		_ ->
			install2(Nodes)
	end.
%% @hidden
install1([Node] = Nodes) when Node == node() ->
	case mnesia:start() of
		ok ->
			error_logger:info_msg("Started mnesia~n"),
			install2(Nodes);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
					{error, Reason}]),
			{error, Reason}
	end;
install1(Nodes) ->
	case rpc:multicall(Nodes, mnesia, start, [], 60000) of
		{Results, []} ->
			F = fun(ok) ->
						false;
					(_) ->
						true
			end,
			case lists:filter(F, Results) of
				[] ->
					error_logger:info_report(["Started mnesia on all nodes",
							{nodes, Nodes}]),
					install2(Nodes);
				NotOKs ->
					error_logger:error_report(["Failed to start mnesia"
							" on all nodes", {nodes, Nodes}, {errors, NotOKs}]),
					{error, NotOKs}
			end;
		{Results, BadNodes} ->
			error_logger:error_report(["Failed to start mnesia"
					" on all nodes", {nodes, Nodes}, {results, Results},
					{badnodes, BadNodes}]),
			{error, {Results, BadNodes}}
	end.
%% @hidden
install2(Nodes) ->
	case mnesia:wait_for_tables([schema], ?WAITFORSCHEMA) of
		ok ->
			install3(Nodes, []);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason};
		{timeout, Tables} ->
			error_logger:error_report(["Timeout waiting for tables",
					{tables, Tables}]),
			{error, timeout}
	end.
%% @hidden
install3(Nodes, Acc) ->
	case mnesia:create_table(use_spec, [{ram_copies, Nodes},
			{attributes, record_info(fields, use_spec)}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new usage specification table.~n"),
			case add_example_usage_specs() of
				ok ->
					error_logger:info_msg("Added example usage specifications.~n"),
					install4(Nodes, [use_spec | Acc]);
				{error, Reason} ->
					{error, Reason}
			end;
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, use_spec}} ->
			error_logger:info_msg("Found existing usage specification table.~n"),
			install4(Nodes, [use_spec | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install4(Nodes, Acc) ->
	case application:load(inets) of
		ok ->
			error_logger:info_msg("Loaded inets.~n"),
			install5(Nodes, Acc);
		{error, {already_loaded, inets}} ->
			install5(Nodes, Acc)
	end.
%% @hidden
install5(Nodes, Acc) ->
	case application:get_env(inets, services) of
		{ok, InetsServices} ->
			install6(Nodes, Acc, InetsServices);
		undefined ->
			error_logger:info_msg("Inets services not defined. "
					"User table not created~n"),
			install10(Nodes, Acc)
	end.
%% @hidden
install6(Nodes, Acc, InetsServices) ->
	case lists:keyfind(httpd, 1, InetsServices) of
		{httpd, HttpdInfo} ->
			install7(Nodes, Acc, lists:keyfind(directory, 1, HttpdInfo));
		false ->
			error_logger:info_msg("Httpd service not defined. "
					"User table not created~n"),
			install10(Nodes, Acc)
	end.
%% @hidden
install7(Nodes, Acc, {directory, {_, DirectoryInfo}}) ->
	case lists:keyfind(auth_type, 1, DirectoryInfo) of
		{auth_type, mnesia} ->
			install8(Nodes, Acc);
		_ ->
			error_logger:info_msg("Auth type not mnesia. "
					"User table not created~n"),
			install10(Nodes, Acc)
	end;
install7(Nodes, Acc, false) ->
	error_logger:info_msg("Auth directory not defined. "
			"User table not created~n"),
	install10(Nodes, Acc).
%% @hidden
install8(Nodes, Acc) ->
	case mnesia:create_table(httpd_user, [{type, bag}, {disc_copies, Nodes},
			{attributes, record_info(fields, httpd_user)}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new httpd_user table.~n"),
			install9(Nodes, [httpd_user | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, httpd_user}} ->
			error_logger:info_msg("Found existing httpd_user table.~n"),
			install9(Nodes, [httpd_user | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
					{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install9(Nodes, Acc) ->
	case mnesia:create_table(httpd_group, [{type, bag},{disc_copies, Nodes},
			{attributes, record_info(fields, httpd_group)}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new httpd_group table.~n"),
			install10(Nodes, [httpd_group | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, httpd_group}} ->
			error_logger:info_msg("Found existing httpd_group table.~n"),
			install10(Nodes, [httpd_group | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install10(_Nodes, Tables) ->
	case mnesia:wait_for_tables(Tables, ?WAITFORTABLES) of
		ok ->
			install11(Tables, lists:member(httpd_user, Tables));
		{timeout, Tables} ->
			error_logger:error_report(["Timeout waiting for tables",
					{tables, Tables}]),
			{error, timeout};
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
					{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install11(Tables, true) ->
	case inets:start() of
		ok ->
			error_logger:info_msg("Started inets.~n"),
			install12(Tables);
		{error, {already_started, inets}} ->
			install12(Tables);
		{error, Reason} ->
			error_logger:error_msg("Failed to start inets~n"),
			{error, Reason}
	end;
install11(Tables, false) ->
	{ok, Tables}.
%% @hidden
install12(Tables) ->
	case usekeeper:list_users() of
		{ok, []} ->
			Username = "admin",
			Password = "admin",
			case usekeeper:add_user(Username, Password) of
				{ok, _} ->
					error_logger:info_report(["Created a default user",
							{username, Username}, {password, Password}]),
					{ok, Tables};
				{error, Reason} ->
					error_logger:error_report(["Failed to creat default user",
							{username, Username}, {password, Password}]),
					{error, Reason}
			end;
		{ok, Users} ->
			error_logger:info_report(["Found existing http users",
					{users, Users}]),
			{ok, Tables};
		{error, Reason} ->
			error_logger:error_report(["Failed to list http users",
				{error, Reason}]),
			{error, Reason}
	end.

-spec start_phase(Phase, StartType, PhaseArgs) -> Result
	when
		Phase :: atom(),
		StartType :: start_type(),
		PhaseArgs :: term(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Called for each start phase in the application and included
%% 	applications.
%% @see //kernel/app
%%
start_phase(_Phase, _StartType, _PhaseArgs) ->
	ok.

-spec prep_stop(State) -> #state{}
	when
		State :: #state{}.
%% @doc Called when the application is about to be shut down,
%% 	before any processes are terminated.
%% @see //kernel/application:stop/1
%%
prep_stop(State) ->
	State.

-spec stop(State) -> any()
	when
		State :: #state{}.
%% @doc Called after the application has stopped to clean up.
%%
stop(_State) ->
	ok.

-spec config_change(Changed, New, Removed) -> ok
	when
		Changed:: [{Par, Val}],
		New :: [{Par, Val}],
		Removed :: [Par],
		Par :: atom(),
		Val :: atom().
%% @doc Called after a code  replacement, if there are any
%% 	changes to the configuration  parameters.
%%
config_change(_Changed, _New, _Removed) ->
	ok.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec force(Tables) -> Result
	when
		Tables :: [TableName],
		Result :: ok | {error, Reason},
		TableName :: atom(),
		Reason :: term().
%% @doc Try to force load bad tables.
force([H | T]) ->
	case mnesia:force_load_table(H) of
		yes ->
			force(T);
		ErrorDescription ->
			{error, ErrorDescription}
	end;
force([]) ->
	ok.

-spec create_dir(ExportDir) -> Result
	when
		ExportDir :: string(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Create the MIB directory.
create_dir(ExportDir) ->
	case file:make_dir(ExportDir) of
		ok ->
			ok;
		{error, eexist} ->
			ok;
		{error, Reason} ->
			{error, Reason}
	end.

-spec add_example_usage_specs() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Seed `use_spec' table with example usage specifications.
%% @private
add_example_usage_specs() ->
	Specification = #use_spec{name = "WLAN",
			description = "IPDR Public WLAN Access - WISP Use Case",
			start_date = 1575158400000, end_date = 1609459199000,
			characteristic = ipdr_wlan()},
	case usekeeper:add_usage_spec(Specification) of
		{ok, #use_spec{}} ->
			ok;
		{error, Reason} ->
			{error, Reason}
	end.

-spec ipdr_wlan() -> map().
%% @doc Usage specification characteristics for IPDR
%% 	Public WLAN Access - WISP Use Case.
ipdr_wlan() ->
	#{"ipdrCreationTime" => #{"name" => "ipdrCreationTime",
				"description" => "", "valueType" => "string"},
		"seqNum" => #{"name" => "seqNum",
				"description" => "",  "valueType" => "integer",
				"specCharacteristicValue" => #{"valueType" => "integer",
						"valueFrom" => 0}},
		"username" => #{"name" => "username",
				"description" => "The end user ID and their domain name (NAI).",
				"valueType" => "string"},
		"scIdType" => #{"name" => "scIdType",
				"description" => "",  "valueType" => "integer",
				"specCharacteristicValue" => #{"valueType" => "integer",
						"valueFrom" => 1, "valueTo" => 3}},
		"scId" => #{"name" => "scId",
				"description" => "", "valueType" => "string"},
		"homeServiceProviderType" => #{"name" => "homeServiceProviderType",
				"description" => "",  "valueType" => "integer",
				"specCharacteristicValue" => #{"valueType" => "integer",
						"valueFrom" => 1, "valueTo" => 4}},
		"homeServiceProvider" => #{"name" => "homeServiceProvider",
				"description" => "", "valueType" => "string"},
		"acctSessionId" => #{"name" => "acctSessionId",
				"description" => "", "valueType" => "string"},
		"userIpAddress" => #{"name" => "userIpAddress",
				"description" => "", "valueType" => "string"},
		"callingStationId" => #{"name" => "callingStationId",
				"description" => "", "valueType" => "string"},
		"calledStationId" => #{"name" => "calledStationId",
				"description" => "", "valueType" => "string"},
		"nasIpAddress" => #{"name" => "nasIpAddress",
				"description" => "", "valueType" => "string"},
		"nasId" => #{"name" => "nasId",
				"description" => "", "valueType" => "string"},
		"accessProviderType" => #{"name" => "accessProviderType",
				"description" => "",  "valueType" => "integer",
				"specCharacteristicValue" => #{"valueType" => "integer",
						"valueFrom" => 1, "valueTo" => 4}},
		"accessServiceProvider" => #{"name" => "accessServiceProvider",
				"description" => "", "valueType" => "string"},
		"locationName" => #{"name" => "locationName",
				"description" => "", "valueType" => "string"},
		"locationId" => #{"name" => "locationId",
				"description" => "", "valueType" => "string"},
		"locationType" => #{"name" => "locationType",
				"description" => "", "valueType" => "string"},
		"locationCountryCode" => #{"name" => "locationCountryCode",
				"description" => "", "valueType" => "string"},
		"locationStateProvince" => #{"name" => "locationStateProvince",
				"description" => "", "valueType" => "string"},
		"locationCity" => #{"name" => "locationCity",
				"description" => "", "valueType" => "string"},
		"locationGeocode" => #{"name" => "locationGeocode",
				"description" => "", "valueType" => "string"},
		"locationGeocodeType" => #{"name" => "locationGeocodeType",
				"description" => "", "valueType" => "string"},
		"nasPortType" => #{"name" => "nasPortType",
				"description" => "",  "valueType" => "integer",
				"specCharacteristicValue" => #{"valueType" => "integer",
						"valueFrom" => 0, "valueTo" => 19}},
		"paymentType" => #{"name" => "paymentType",
				"description" => "",  "valueType" => "integer",
				"specCharacteristicValue" => #{"valueType" => "integer",
						"valueFrom" => 1, "valueTo" => 3}},
		"networkConnectionType" => #{"name" => "networkConnectionType",
				"description" => "", "valueType" => "string"},
		"sessionDuration" => #{"name" => "sessionDuration",
				"description" => "",  "valueType" => "integer",
				"specCharacteristicValue" => #{"valueType" => "integer",
						"valueFrom" => 0}},
		"inputOctets" => #{"name" => "inputOctets",
				"description" => "",  "valueType" => "integer",
				"specCharacteristicValue" => #{"valueType" => "integer",
						"valueFrom" => 0}},
		"outputOctets" => #{"name" => "outputOctets",
				"description" => "",  "valueType" => "integer",
				"specCharacteristicValue" => #{"valueType" => "integer",
						"valueFrom" => 1}},
		"class" => #{"name" => "class",
				"description" => "", "valueType" => "string"},
		"gmtSessionStartDateTime" => #{"name" => "gmtSessionStartDateTime",
				"description" => "", "valueType" => "string"},
		"gmtSessionEndDateTime" => #{"name" => "gmtSessionStartDateTime",
				"description" => "", "valueType" => "string"},
		"sessionTerminateCause" => #{"name" => "sessionTerminateCause",
				"description" => "",  "valueType" => "integer",
				"specCharacteristicValue" => #{"valueType" => "integer",
						"valueFrom" => 1, "valueTo" => 7}},
		"billingClassOfService" => #{"name" => "billingClassOfService",
				"description" => "", "valueType" => "string"},
		"unitOfMeasure" => #{"name" => "unitOfMeasure",
				"description" => "",  "valueType" => "integer",
				"specCharacteristicValue" => #{"valueType" => "integer",
						"valueFrom" => 1, "valueTo" => 7}},
		"chargeableUnit" => #{"name" => "chargeableUnit",
				"description" => "",  "valueType" => "integer",
				"specCharacteristicValue" => #{"valueType" => "integer",
						"valueFrom" => 1, "valueTo" => 7}},
		"chargeableQuantity" => #{"name" => "chargeableQuantity",
				"description" => "",  "valueType" => "integer",
				"specCharacteristicValue" => #{"valueType" => "integer",
						"valueFrom" => 0}},
		"chargeAmount" => #{"name" => "chargeAmount",
				"description" => "",  "valueType" => "integer",
				"specCharacteristicValue" => #{"valueType" => "integer",
						"valueFrom" => 0}},
		"chargeCurrencyType" => #{"name" => "chargeCurrencyType",
				"description" => "", "valueType" => "string"},
		"otherParty" => #{"name" => "otherParty",
				"description" => "", "valueType" => "string"},
		"taxPercentage" => #{"name" => "taxPercentage",
				"description" => "",  "valueType" => "integer",
				"specCharacteristicValue" => #{"valueType" => "integer",
						"valueFrom" => 0, "valueTo" => 100}},
		"taxAmount" => #{"name" => "taxAmount",
				"description" => "",  "valueType" => "integer",
				"specCharacteristicValue" => #{"valueType" => "integer",
						"valueFrom" => 0}},
		"taxType" => #{"name" => "taxType",
				"description" => "",  "valueType" => "integer",
				"specCharacteristicValue" => #{"valueType" => "integer",
						"valueFrom" => 1, "valueTo" => 14}},
		"intermediaryName" => #{"name" => "intermediaryName",
				"description" => "", "valueType" => "string"},
		"serviceName" => #{"name" => "serviceName",
				"description" => "",  "valueType" => "integer",
				"specCharacteristicValue" => #{"valueType" => "integer",
						"valueFrom" => 1, "valueTo" => 6}},
		"relatedIpdrIdList" => #{"name" => "relatedIpdrIdList",
				"description" => "", "valueType" => "string"},
		"tempUserId" => #{"name" => "tempUserId",
				"description" => "", "valueType" => "string"}}.

