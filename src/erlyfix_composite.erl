-module(erlyfix_composite).
-include("erlyfix_records.hrl").

-export([decompose/1]).

decompose(C) when is_record(C, field) -> {field, C};
decompose(C) when is_record(C, group) -> {group, C#group.name, C#group.composite4name, C#group.mandatoryComposites};
decompose(C) when is_record(C, component) -> {composite, C#component.name, C#component.composite4name, C#component.mandatoryComposites };
decompose(C) when is_record(C, header) -> {composite, header, C#header.composite4name, C#header.mandatoryComposites };
decompose(C) when is_record(C, trailer) -> {composite, trailer, C#trailer.composite4name, C#trailer.mandatoryComposites }.
