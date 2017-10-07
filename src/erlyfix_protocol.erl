-module(erlyfix_protocol).
-include("erlyfix_records.hrl").

-export([load/1, version/1, lookup/2]).

%% XML-parsing

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
            case Value of
                "Y" -> {ok, true};
                "N" -> {ok, false}
            end;
        {not_found} -> {not_found}
    end.

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
            {ok, Reqired} = find_required(Attributes),
            [ [], {group_def, Name, Reqired} | Acc ];
        {endElement,[],"group",[]} ->
            [Composites, {group_def, Name, Required} | Acc1] = Acc,
            GroupDef = #group_def { name = Name, required = Required, composites = Composites},
            [H | T1] = Acc1,
            [ [GroupDef | H] | T1 ];
        {startElement, [], "value", [], Attributes} ->
            {ok, Key} = find_attr("enum", Attributes),
            {ok, Description} = find_attr("description", Attributes),
            ValueDef = #value_def{ key = Key, description = Description},
            [H | T] = Acc,
            [ [ValueDef | H] | T ];
        _ -> Acc
    end.

%% Constuction

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

construct_composite_mapping({Co4Name, MC}, []) -> {Co4Name, MC};
construct_composite_mapping({Co4Name, MC}, [{Ref, Def} | T ]) ->
    {Name, Required} = case element(1, Ref) of
        field_ref -> { Ref#field_ref.name, Ref#field_ref.required };
        component_ref -> { Ref#component_ref.name, Ref#component_ref.required };
        group_def -> { Ref#group_def.name, Ref#group_def.required }
    end,
    Co4NameNew = Co4Name#{ Name => Def },
    MCNew = case Required of
        true -> MC#{ Name => Def};
        false -> MC
    end,
    construct_composite_mapping({Co4NameNew, MCNew}, T).

zip(Refs, ReverseDefNames) ->
    F = fun({_Name, Composite}) -> Composite end,
    Defs = lists:reverse( lists:map(F, ReverseDefNames)),
    RefsDefs = lists:zip(Refs, Defs),
    {Co4Name, MC} = construct_composite_mapping({ #{}, #{} }, RefsDefs),
    {Co4Name, MC}.

get_composites(Acc, _C4Name, _F4Name, []) -> {ok, Acc};
get_composites(Acc, C4Name, F4Name, [H | T]) ->
    case element(1, H) of
        component_ref ->
            Name = H#component_ref.name,
            case maps:find(Name, C4Name) of
                {ok, Composite} -> get_composites([{Name, Composite} | Acc], C4Name, F4Name, T);
                error -> not_found
            end;
        field_ref ->
            Name = H#field_ref.name,
            case maps:find(Name, F4Name) of
                {ok, Composite} -> get_composites([{Name, Composite} | Acc], C4Name, F4Name, T);
                error -> not_found
            end;
        group_def ->
            Name = H#group_def.name,
            CompositeRefs = H#group_def.composites,
            case get_composites([], C4Name, F4Name, CompositeRefs) of
                {ok, SubCompositesNames} ->
                    {Co4Name, MC} = zip(CompositeRefs, SubCompositesNames),
                    Composite = #group{ name = Name, composite4name = Co4Name, mandatoryComposites = MC},
                    get_composites([{Name, Composite} | Acc], C4Name, F4Name, T);
                not_found -> not_found
            end
    end.

construct_components(C4Name, F4Name, Queue) ->
    case queue:is_empty(Queue) of
        true -> C4Name;
        false ->
            { {value, ComponentRef}, QLeft} = queue:out(Queue),
            %io:format("trying to construct composite ~p~n", [ComponentRef]),
            #component_def{ name = Name, composites = SubCompositeRefs } = ComponentRef,
            case get_composites([], C4Name, F4Name, SubCompositeRefs) of
                {ok, SubCompositesNames} ->
                    %io:format("found subcomposites ~p :: ~p~n", [ComponentRef, SubCompositesNames]),
                    {Co4Name, MC} = zip(SubCompositeRefs, SubCompositesNames),
                    Composite = #component{ name = Name, composite4name = Co4Name, mandatoryComposites = MC},
                    C4NameNew = C4Name#{ Name => Composite },
                    construct_components(C4NameNew, F4Name, QLeft);
                not_found ->
                    Q2 = queue:in(ComponentRef, QLeft),
                    construct_components(C4Name, F4Name, Q2)
            end
    end.


construct(Map) ->
    % fields
    FieldDefinitions = maps:get(fields, Map),
    ComponentDefinitions = maps:get(components, Map),
    {Field4Name, Field4Number} = construct_fields({ #{}, #{}}, FieldDefinitions),

    % components
    ComponentsQueue = queue:from_list(ComponentDefinitions),
    C4Name = construct_components(#{}, Field4Name, ComponentsQueue),

    % header
    HeaderRefs = maps:get(header, Map),
    {ok, HeaderCompositeNames} = get_composites([], C4Name, Field4Name, HeaderRefs),
    {H_C4Name, H_MC} = zip(HeaderRefs, HeaderCompositeNames),
    Header = #header{ composite4name = H_C4Name, mandatoryComposites = H_MC },

    % trailer
    TrailerRefs = maps:get(trailer, Map),
    {ok, TrailerCompositeNames} = get_composites([], C4Name, Field4Name, TrailerRefs),
    {T_C4Name, T_MC} = zip(TrailerRefs, TrailerCompositeNames),
    Trailer = #trailer{ composite4name = T_C4Name, mandatoryComposites = T_MC },

    #protocol{
        protocol_version = maps:get(version, Map),
        header           = Header,
        trailer          = Trailer,
        field4name       = Field4Name,
        field4number     = Field4Number,
        component4name   = C4Name}.

%% Interface method

load(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
          {ok, Map, _} = erlsom:parse_sax(Bin, #{}, fun callback/2),
          Protocol = construct(Map),
          Protocol;
          %Map;
        Error -> Error
    end.

version(Protocol)-> Protocol#protocol.protocol_version.

% lookup
lookup(Protocol, {field, by_name, K}) ->
    L = Protocol#protocol.field4name,
    case maps:find(K, L) of
        {ok, Field} -> {ok, Field};
        error -> not_found
    end;

lookup(Protocol, {field, by_number, K}) ->
    L = Protocol#protocol.field4number,
    case maps:find(K, L) of
        {ok, Field} -> {ok, Field};
        error -> not_found
    end;

lookup(Protocol, {component, K}) ->
    L = Protocol#protocol.component4name,
    case maps:find(K, L) of
        {ok, C} -> {ok, C};
        error -> not_found
    end.

