-module(erlyfix_composite).
-include("erlyfix.hrl").

-export([name/1]).

name(C) when is_record(C, field) -> C#field.name;
name(C) when is_record(C, group) -> C#group.name;
name(C) when is_record(C, component) -> C#component.name.
