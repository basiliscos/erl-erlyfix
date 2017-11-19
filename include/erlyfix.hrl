-record(protocol_version, {
    major        :: integer(),
    minor        :: integer(),
    servicepack  :: integer()
}).
-type protocol_version() :: #protocol_version{}.

%% Definitions

-record(value_def, {
    key         :: atom(),
    description :: binary()
}).
-type value_def() :: #value_def{}.

-record(field_def, {
    name      :: atom(),
    number    :: non_neg_integer(),
    type      :: atom(),
    values    :: [value_def()]
}).
-type field_def() :: #field_def{}.

-record(component_def, {
    name       :: atom(),
    composites :: [composite_ref()]
}).
-type component_def() :: #component_def{}.

-record(group_def, {
    name       :: atom(),
    composites :: [composite_ref()],
    required   :: true|false
}).
-type group_def() :: #group_def{}.

%% References

-record(field_ref, {
    name      :: atom(),
    required  :: true|false
}).
-type field_ref() :: #field_ref{}.

-record(component_ref, {
    name       :: atom(),
    required   :: true|false
}).
-type component_ref() :: #component_ref{}.


-type composite_ref() :: field_ref() | group_def() | component_ref().

-record(message_ref, {
    name       :: atom(),
    type       :: atom(),
    category   :: atom(),
    composites :: [composite_ref()]
}).

%% Objects

-record(field, {
    id                  :: non_neg_integer(),
    name                :: atom(),
    number              :: non_neg_integer(),
    type                :: atom(),
    value4key           :: map(),
    value4description   :: map(),
    composite4field     :: map()
}).
-type field() :: #field{}.

-record(component, {
    id                  :: non_neg_integer(),
    name                :: atom(),
    composite4name      :: map(),
    mandatoryComposites :: map(),
    composite4field     :: map()
}).
-type component() :: #component{}.

-record(group, {
    id                  :: non_neg_integer(),
    name                :: atom(),
    composite4name      :: map(),
    mandatoryComposites :: map(),
    composite4field     :: map()
}).
-type group() :: #group{}.

-record(header, {
    composite4name      :: map(),
    mandatoryComposites :: map(),
    composite4field     :: map()
}).
-type header() :: #header{}.

-record(trailer, {
    composite4name      :: map(),
    mandatoryComposites :: map(),
    composite4field     :: map()
}).
-type trailer() :: #trailer{}.

-record(message, {
    name                :: atom(),
    type                :: atom(),
    category            :: atom(),
    composite4name      :: map(),
    mandatoryComposites :: map(),
    composite4field     :: map()
}).
-type message() :: #message{}.

-record(field_REs, {
    data_separator      :: re:mp(),
    tag_separator       :: re:mp(),
    digits              :: re:mp(),
    int                 :: re:mp(),
    float               :: re:mp(),
    country             :: re:mp(),
    boolean             :: re:mp(),
    utctimeonly         :: re:mp(),
    monthyear           :: re:mp(),
    localmktdate        :: re:mp(),
    utctimestamp        :: re:mp()
}).
-type field_REs() :: #field_REs{}.

-record(parser_helpers, {
    checksum_digits     :: re:mp(),
    checksum_size       :: non_neg_integer(),
    begin_string        :: binary(),
    field_REs           :: field_REs()
}).
-type parser_helpers() :: #parser_helpers{}.

-record(uncompiled_protocol, {
    protocol_version    :: protocol_version(),
    header              :: header(),
    trailer             :: trailer(),
    field4number        :: map(),
    field4name          :: map(),
    component4name      :: map(),
    message4name        :: map(),
    message4type        :: map(),
    container           :: array:array()
}).
-type uncompiled_protocol() :: #uncompiled_protocol{}.

-record(protocol, {
    protocol_version    :: protocol_version(),
    header              :: header(),
    trailer             :: trailer(),
    field4number        :: map(),
    field4name          :: map(),
    component4name      :: map(),
    message4name        :: map(),
    message4type        :: map(),
    container           :: array:array(),
    parser_helpers      :: parser_helpers()
}).
-type protocol() :: #protocol{}.

% field types

-record(monthyear_week, {
    year    :: non_neg_integer(),
    month   :: non_neg_integer(),
    week    :: non_neg_integer()
}).

-record(monthyear_day, {
    year    :: non_neg_integer(),
    month   :: non_neg_integer(),
    day     :: non_neg_integer()
}).

-type monthyear() :: #monthyear_week{} | #monthyear_day{}.

-record(utc_time, {
    hour     :: non_neg_integer(),
    minute   :: non_neg_integer(),
    second   :: non_neg_integer(),
    ms       :: non_neg_integer()
}).
-type utc_time() :: #utc_time{}.

-record(fix_date, {
    year    :: non_neg_integer(),
    month   :: non_neg_integer(),
    day     :: non_neg_integer()
}).
-type fix_date() :: #fix_date{}.

-record(utc_timestamp, {
    date :: fix_date(),
    time :: utc_time()
}).
-type utc_timestamp() :: #utc_timestamp{}.
