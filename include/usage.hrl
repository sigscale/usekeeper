%%% usage.hrl

-type use_status() :: received | rejected | recycled | guided | rated | rerate | billed.

-type related_party_ref() :: #{AttributeName :: string()
		:= AttributeValue :: string() | pos_integer()}.

-type specification_ref() :: #{AttributeName :: string()
		:= AttributeValue :: string()}.

-type specification_char() :: #{AttributeName :: string()
		:= AttributeValue :: string() | non_neg_integer() | boolean() | list()}.

-type rated_product_usage() :: #{AttributeName :: string()
		:= AttributeValue :: string() | non_neg_integer() | boolean()}.

-record(use_spec,
		{id :: string() | undefined,
		name :: string() | undefined,
		description :: string() | undefined,
		class_type :: string() | undefined,
		base_type :: string() | undefined,
		schema :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		last_modified :: {TS :: pos_integer(), N :: pos_integer()} | undefined,
		characteristic = #{} :: #{Name :: string() := specification_char()}}).
-type use_spec() :: #use_spec{}.

-type usage() :: #{AttributeName :: string()
		:= AttributeValue :: string() | pos_integer() | use_status()
		| specification_ref() | rated_product_usage() | map()}.

-type usage_log() :: {TS :: pos_integer(), N :: pos_integer(), Usage :: usage()}.
