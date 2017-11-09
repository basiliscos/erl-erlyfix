-record(protocol_version, {
    major        :: integer(),
    minor        :: integer(),
    servicepack  :: integer()
}).
-type protocol_version() :: #protocol_version{}.

%% Definitions

-record(value_def, {
    key         :: string(),
    description :: string()
}).
-type value_def() :: #value_def{}.

-record(field_def, {
    name      :: string(),
    number    :: integer(),
    type      :: string(),
    values    :: [value_def()]
}).
-type field_def() :: #field_def{}.

-record(component_def, {
    name       :: string(),
    composites :: [composite_ref()]
}).
-type component_def() :: #component_def{}.

-record(group_def, {
    name       :: string(),
    composites :: [composite_ref()],
    required   :: true|false
}).
-type group_def() :: #group_def{}.

%% References

-record(field_ref, {
    name      :: string(),
    required  :: true|false
}).
-type field_ref() :: #field_ref{}.

-record(component_ref, {
    name       :: string(),
    required   :: true|false
}).
-type component_ref() :: #component_ref{}.


-type composite_ref() :: field_ref() | group_def() | component_ref().

-record(message_ref, {
    name       :: string(),
    type       :: string(),
    category   :: string(),
    composites :: [composite_ref()]
}).

%% Objects

-record(field, {
    name                :: string(),
    number              :: integer(),
    type                :: string(),
    value4key           :: map(),
    value4description   :: map(),
    composite4field     :: map()
}).
-type field() :: #field{}.

-record(component, {
    name                :: string(),
    composite4name      :: map(),
    mandatoryComposites :: map(),
    composite4field     :: map()
}).
-type component() :: #component{}.

-record(group, {
    name                :: string(),
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
    name                :: string(),
    type                :: string(),
    category            :: string(),
    composite4name      :: map(),
    mandatoryComposites :: map(),
    composite4field     :: map()
}).
-type message() :: #message{}.


-record(protocol, {
    protocol_version    :: protocol_version(),
    header              :: header(),
    trailer             :: trailer(),
    field4number        :: map(),
    field4name          :: map(),
    component4name      :: map(),
    message4name        :: map(),
    message4type        :: map()
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
