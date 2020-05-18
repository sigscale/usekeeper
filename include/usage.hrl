%%% usage.hrl

-type use_status() :: received | rejected | recycled | guided | rated | rerate | billed.
-type rating_tag() :: usage | included_usage | non_included_usage.

-type related_party_ref() :: #{id := string(),
		href := string(),
		name => string(),
		type => string(),
		base_type => string(),
		schema_location => string(),
		referred_type => string(),
		role => string()}.

-type specification_ref() :: #{id := string(),
		href := string(),
		name => string(),
		type => string(),
		base_type => string(),
		schema_location => string(),
		referred_type => string()}.

-type specification_char() :: #{AttributeName :: string()
		:= AttributeValue :: string() | non_neg_integer() | boolean() | list()}.

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
		characteristic = #{} :: #{Name :: string() := Chars :: specification_char()}}).
-type use_spec() :: #use_spec{}.

-type usage_char() :: #{Name :: string() := Value :: string() | list() | integer()
		| float() | boolean() | map()}.

-type product_ref() :: #{id := string(),
		href := string(),
		name => string(),
		type => string(),
		base_type => string(),
		schema_location => string(),
		referred_type => string(),
		role => string()}.

-type rated_product_usage() :: #{id := string(),
		href => string(),
		name => string(),
		type => string(),
		base_type => string(),
		schema_location => string(),
		bucket_converted_in_amount => integer(),
		currency_code => string(),
		is_billed => boolean(),
		is_tax_exempt => boolean(),
		tariff_type => string(),
		productRef => product_ref() | string(),
		rating_amount_type => string(),
		rating_date => integer(),
		tax_excluded_amount => integer(),
		tax_included_amount => integer(),
		tax_rate => integer(),
		rating_tag => rating_tag()}.

-type usage() :: #{id := string(),
		href := string(),
		type => string(),
		base_type => string(),
		schema_location => string(),
		date := integer(),
		usage_type := string(),
		description => string(),
		status := use_status(),
		usage_specification := specification_ref(),
		usage_characteristic := #{Name :: string() := Chars :: usage_char()},
		related_party => #{Name :: string() := RelatedPaty :: related_party_ref()},
		rated_usage => #{Name :: string() := RatedUsage :: rated_product_usage()}}.

-type usage_log() :: {TS :: pos_integer(), N :: pos_integer(), Usage :: usage()}.

