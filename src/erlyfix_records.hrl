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
    value4description   :: map()
}).
-type field() :: #field{}.

-record(component, {
    name                :: string(),
    composite4name      :: map(),
    mandatoryComposites :: map()
}).
-type component() :: #component{}.

-record(group, {
    name                :: string(),
    composite4name      :: map(),
    mandatoryComposites :: map()
}).
-type group() :: #group{}.


-record(protocol, {
    field4number        :: map(),
    field4name          :: map(),
    component4name      :: map(),
    protocol_version    :: protocol_version()
}).

