-record(protocol_version, {
    major        :: integer(),
    minor        :: integer(),
    servicepack  :: integer()
}).
-type protocol_version() :: #protocol_version{}.

-record(field_ref, {
    name      :: string(),
    required  :: true|false
}).

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


-record(component_ref, {
    name       :: string(),
    composites :: [composite_ref()]
}).

-record(group_ref, {
    name       :: string(),
    composites :: [composite_ref()]
}).


-type field_ref() :: #field_ref{}.
-type composite_ref() :: field_ref().

-record(message_ref, {
    name       :: string(),
    type       :: string(),
    category   :: string(),
    composites :: [composite_ref()]
}).
