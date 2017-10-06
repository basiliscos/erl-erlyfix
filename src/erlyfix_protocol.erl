-module(erlyfix_protocol).
-include("erlyfix_records.hrl").

-export([load/1]).

find_attr(Name, Attributes) ->
    F = fun(I) ->
        case I of
             {attribute,Name, _, _, _} -> true;
             _ -> false
         end
    end,
    case lists:filter(F, Attributes) of
        [{attribute,Name, _, _, Value}] -> {ok, Value};
        _ -> {not_found}
    end.

find_required(Attributes) ->
    case find_attr("required", Attributes) of
        {ok, Value} ->
            Required = case Value of
                "Y" -> {ok, true};
                "N" -> {ok, false}
            end;
        {not_found} -> {not_found}
    end.
%
callback(Event, Acc) ->
    %io:format("~p~n", [Event]),
    case Event of
        {startElement, [], "fix", [], Attributes} ->
            %io:format("~p~n", [Acc]),
            {ok, Minor} = find_attr("minor", Attributes),
            {ok, Major} = find_attr("major", Attributes),
            {ok, Servicepack} = find_attr("servicepack", Attributes),
            {ok,"FIX"} = find_attr("type", Attributes),
            Version = #protocol_version {
                major = list_to_integer(Major),
                minor = list_to_integer(Minor),
                servicepack = list_to_integer(Servicepack)},
            Acc#{ version => Version };
        {startElement,[],"header",[],[]} -> [ [] | Acc ];
        {endElement,[],"header",[]} -> [H | T] = Acc, T#{ header => H };
        {startElement,[],"trailer",[],[]} -> [ [] | Acc ];
        {endElement,[],"trailer",[]} -> [H | T] = Acc, T#{ trailer => H };
        {startElement,[],"messages",[],[]} -> [ [] | Acc ];
        {endElement,[],"messages",[]} -> [H | T] = Acc, T#{ messages => H };
        {startElement,[],"components",[],[]} -> [ [] | Acc ];
        {endElement,[],"components",[]} -> [H | T] = Acc, T#{ components => H };
        {startElement,[],"fields",[],[]} -> [ [] | Acc ];
        {endElement,[],"fields",[]} -> [H | T] = Acc, T#{ fields => H };
        {startElement, [], "field", [], Attributes} ->
            [H | T] = Acc,
            {ok, Name} = find_attr("name", Attributes),
            case find_required(Attributes) of
                {ok, Required} ->                   % reference
                    FieldRef = #field_ref{ name = Name, required = Required },
                    [ {field_ref, FieldRef}, H | T ];
                {not_found} ->                      % definition
                    {ok, Number} = find_attr("number", Attributes),
                    {ok, Type} = find_attr("type", Attributes),
                    [ [], {field_def, Name, Number, Type, H} | T ]
                    %Acc
            end;
        {endElement,[],"field",[]} ->
            [F, H | T] = Acc,
            case is_list(F) of
                false ->                            % reference
                    {field_ref, FieldRef} = F,
                    [ [FieldRef | H] | T ];
                true ->                             % definition
                    {field_def, Name, Number, Type, H2} = H,
                    FieldDef = #field_def {
                        name = Name,
                        number = Number,
                        type = Type,
                        values = F},
                    [ [FieldDef | H2] | T ]
            end;
        {startElement, [], "message", [], Attributes} ->
            %io:format("message :: ~p~n", [Attributes]),
            {ok, Name} = find_attr("name", Attributes),
            {ok, Type} = find_attr("msgtype", Attributes),
            {ok, Category} = find_attr("msgcat", Attributes),
            [ [], Name, Type, Category | Acc ];
        {endElement,[],"message",[]} ->
            [Composites, Name, Type, Category | Acc1] = Acc,
            MessageRef = #message_ref {
                name = Name,
                type = Type,
                category = Category,
                composites = Composites},
            [H | T1] = Acc1,
            [ [MessageRef | H] | T1 ];
        {startElement, [], "component", [], Attributes} ->
            [H | T] = Acc,
            {ok, Name} = find_attr("name", Attributes),
            case find_required(Attributes) of
                {ok, Required} ->                   % reference
                    ComponentRef = #component_ref{ name = Name, required = Required },
                    [ {component_ref, ComponentRef}, H | T ];
                {not_found} ->                      % definition
                    [ [], {component_def, Name, H} | T ]
            end;
        {endElement,[],"component",[]} ->
            [F, H | T] = Acc,
            case is_list(F) of
                false ->                            % reference
                    {component_ref, ComponentRef} = F,
                    [ [ComponentRef | H] | T ];
                true ->                             % definition
                    {component_def, Name, H2} = H,
                    ComponentDef = #component_def {
                        name = Name,
                        composites = F},
                    [ [ComponentDef | H2] | T ]
            end;
        {startElement, [], "group", [], Attributes} ->
            {ok, Name} = find_attr("name", Attributes),
            [ [], Name | Acc ];
        {endElement,[],"group",[]} ->
            [Composites, Name | Acc1] = Acc,
            GroupRef = #group_ref { name = Name, composites = Composites},
            [H | T1] = Acc1,
            [ [GroupRef | H] | T1 ];
        {startElement, [], "value", [], Attributes} ->
            {ok, Key} = find_attr("enum", Attributes),
            {ok, Description} = find_attr("description", Attributes),
            ValueDef = #value_def{ key = Key, description = Description},
            [H | T] = Acc,
            [ [ValueDef | H] | T ];
        _ -> Acc
    end.

map_values(Acc, []) -> Acc;
map_values({Value4Key, Value4Description}, [Value | Rest]) ->
    K = Value#value_def.key,
    D = Value#value_def.description,
    Acc = {
        Value4Key#{K => Value},
        Value4Description#{D => Value}
    },
    map_values(Acc, Rest).


construct_fields(Acc, []) -> Acc;
construct_fields({Field4Name, Field4Number}, [FieldDefinition | Rest]) ->
    ValuesList = FieldDefinition#field_def.values,
    {Value4Key, Value4Description} = map_values({ #{}, #{} }, ValuesList),
    Name = FieldDefinition#field_def.name,
    Number = FieldDefinition#field_def.number,
    Field = #field{
        name   = Name,
        number = Number,
        type   = FieldDefinition#field_def.type,
        value4key = Value4Key,
        value4description = Value4Description},
    Acc = {
        Field4Name#{ Name => Field },
        Field4Number#{ Number => Field}
    },
    construct_fields(Acc, Rest).

get_composites(Acc, _Lookup, []) -> {ok, Acc};
get_composites(Acc, L, [H | T]) ->
    {C4Name, G4Name, F4Name} = L,
    {Name, Lookup} = case element(1, H) of
        component -> {H#component.name, C4Name};
        group     -> {H#group.name, G4Name};
        field     -> {H#field.name, F4Name}
    end,
    case maps:find(Name, Lookup) of
        {ok, Composite} -> get_composites([{Name, Composite} | Acc], L, T);
        error -> not_found
    end.

construct_components({C4Name, G4Name}, F4Name, Queue) ->
    Lookup = {C4Name, G4Name, F4Name},
    case queue:is_empty(Queue) of
        true -> {C4Name, G4Name};
        false ->
            { {value, ComponentRef}, QLeft} = queue:out(Queue),
            #component_def{ name = Name, composites = SubCompositeRefs } = ComponentRef,
            case get_composites([], Lookup, SubCompositeRefs) of
                {ok, Values} ->
                    1;
                not_found ->
                    Q2 = queue:in(ComponentRef, Queue),
                    construct_components({C4Name, G4Name}, F4Name, Q2)
            end
    end.


construct(Map) ->
    FieldDefinitions = maps:get(fields, Map),
    ComponentDefinitions = maps:get(fields, Map),
    {Field4Name, Field4Number} = construct_fields({ #{}, #{}}, FieldDefinitions),
    ComponentsQueue = queue:from_list(ComponentDefinitions),
    Field4Name.

load(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
          {ok, Map, _} = erlsom:parse_sax(Bin, #{}, fun callback/2),
          %Protocol = construct(Map),
          %Protocol;
          Map;
        Error ->
          Error
    end.
