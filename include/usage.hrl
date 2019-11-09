%%% usage.hrl

-type use_status() :: received | rejected | recycled | guided | rated | rerate | billed.

-record(related_party_ref,
		{id :: string() | undefined,
		href :: string() | undefined,
		name :: string() | undefined,
		role :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined}).
-type related_party_ref() :: #related_party_ref{}.

-record(specification_ref,
		{id :: string() | undefined,
		href :: string() | undefined,
		name :: string() | undefined,
		version :: string() | undefined}).
-type specification_ref() :: #specification_ref{}.

-record(spec_char_rel,
		{id :: string() | undefined,
		href :: string() | undefined,
		name :: string() | undefined,
		type :: string() | undefined,
		class_type :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined}).
-type spec_char_rel() :: #spec_char_rel{}.

-record(spec_char_value,
		{value_type :: string() | undefined,
		default = false :: boolean() | undefined,
		class_type :: string() | undefined,
		schema :: string() | undefined,
		unit :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		from :: term() | undefined,
		to :: term() | undefined,
		interval :: open | closed | closed_bottom | closed_top | undefined,
		regex :: {CompiledRegEx :: re:mp(), OriginalRegEx :: string()} | undefined,
		value :: term() | undefined}).
-type spec_char_value() :: #spec_char_value{}.

-record(specification_char,
		{name :: string() | undefined,
		description :: string() | undefined,
		value_type :: string() | undefined,
		class_type :: string() | undefined,
		schema :: string() | undefined,
		value_schema :: string() | undefined,
		configurable :: boolean() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		min :: non_neg_integer() | undefined,
		max :: non_neg_integer() | undefined,
		unique :: boolean() | undefined,
		regex :: {CompiledRegEx :: re:mp(), OriginalRegEx :: string()} | undefined,
		extensible :: boolean() | undefined,
		char_relation = [] :: [spec_char_rel()],
		char_value = [] :: [spec_char_value()]}).
-type specification_char() :: #specification_char{}.

-record(use_spec,
		{id :: string() | undefined,
		href :: string() | undefined,
		name :: string() | undefined,
		description :: string() | undefined,
		class_type :: string() | undefined,
		base_type :: string() | undefined,
		schema :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		last_modified :: {TS :: pos_integer(), N :: pos_integer()},
		characteristic = [] :: [specification_char()]}).
-type use_spec() :: #use_spec{}.

-record(usage_char,
		{name :: string() | undefined,
		class_type :: string() | undefined,
		schema :: string() | undefined,
		value :: term() | undefined}).
-type usage_char() :: #usage_char{}.

-record(rated_product_usage,
		{name :: string() | undefined,
		class_type :: string() | undefined,
		schema :: string() | undefined,
		value :: term() | undefined,
		rating_date :: pos_integer() | undefined,
		tag :: usage | included | not_included | undefined,
		amount_type :: string() | undefined,
      tax_included_amount :: integer() | undefined,
		tax_excluded_amount :: integer() | undefined,
		tax_rate :: integer() | undefined,
		is_tax_exempt :: boolean() | undefined,
		tariff_type :: string() | undefined,
		bucket_value :: integer() | undefined,
		currency :: string()}).
-type rated_product_usage() :: #rated_product_usage{}.

-record(usage,
		{id :: string() | undefined,
		href :: string() | undefined,
		name :: string() | undefined,
		description :: string() | undefined,
		class_type :: string() | undefined,
		base_type :: string() | undefined,
		schema :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		status :: use_status() | undefined,
		type :: string() | undefined,
		last_modified :: {TS :: pos_integer(), N :: pos_integer()},
		specification :: specification_ref() | undefined,
		characteristic = [] :: [usage_char()],
		related_party = [] :: [related_party_ref()],
		rated = [] :: [rated_product_usage()]}).
-type usage() :: #usage{}.

